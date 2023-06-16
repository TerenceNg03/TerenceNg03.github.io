# Introduction to Monad Transformers

Before we start, I want to use an example to illustrate why monad transformer is introduced and what problem it is trying to solve.

## Monad Stack

Sometimes we have some basic monads like `IO`, `Maybe` and `State`. Each of them handles their own stuff. But what if I want to return something might not exists in a `State` monad? Intuitively, we will write something like this.

```haskell
run :: State (Maybe String) Int
run = do
    s1 <- get
    case s1 of
        Nothing -> ...
        Just x -> do
            put $ Just "abcde"
            s2 <- get
            case s2 of
                Nothing -> ...
                Just x' -> ...
```

But the problem is every time we try to get the state, we have to do a pattern matching. The code is purely boilerplate because we already have `Maybe` monad to continue calculation only if there is something. We want something that can combine the powers of different monads together. And that is what I am talking about today, monad transformers.

## What is a Monad Transformer

A monad transformer is a `Monad` while is also being an instance of class `MonadTrans`. Let us take a look of their type signatures first.

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

class MonadTrans (t :: (* -> *) -> * -> *) where
    lift :: Monad m => m a -> t m a
```

You may see that `MonadTrans` takes a `* -> *` and a `*` then gives us a real `Type`. It only has only method called `lift` which is very interesting. It takes a monad `m` (corresponding to `* -> *`) then wrapped it into it self (`t`) so we get a `t m a`.

Typically, a monad transformer is also a monad. Which means we can do this
```haskell
run = do 
    config <- ask
    lift $ print config
```

This piece of code will `lift` a `Reader` into an `IO` monad while let us easily get what is inside the monad. Wonderful!

## Implement ReaderT

`ReaderT` is a very simple monad transformer. It takes a **read-only config** and pass along the monad while allow you to query the config with `ask`.

```haskell
import Control.Monad.Trans (MonadTrans, lift)

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
    fmap f rt = ReaderT $ fmap f . runReaderT rt

instance (Applicative m) => Applicative (ReaderT r m) where
    pure x = ReaderT $ const (pure x)
    f <*> rt = ReaderT $ 
        \r -> runReaderT f r <*> runReaderT rt r

instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    rt >>= f = ReaderT $
        \r -> runReaderT rt r >>= (flip runReaderT r <$> f)

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const m
```

And we need to implement something to get our `r` out.

```haskell
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
```

Sometimes we just want use a single `Reader`, so let us make `Reader` be an type alias for apply `ReaderT` on the `Identity` monad.

```haskell
import Control.Monad.Identity (Identity)
type Reader r = ReaderT r Identity
```

## Using a ReaderT

Now that we have `ReaderT` defined, it is time to experiment with it a little bit.

```haskell
run :: ReaderT Int (State String) ()
run = do
    lift $ put "Hello"
    config <- ask
    case config of
        0 -> return ()
        n -> lift $ modify (++ replicate n 'a')
```

As you can see, we are able to read our config anywhere in the do notation block. But the cost is we have to put a lift at everything else which almost make our `ReaderT` unusable. Is there a way to improve this?

## THe MonadX Class

The most oblivious solution is make an alias for `lift put`.

```haskell
putLifted = lift put
putLiftedLifted = lift putLifted
putLiftedLiftedLifted = lift putLiftedLifted
```

Quite tedious, right?

Let us check what `put` really is.
```haskell
class Monad m => MonadState s m | m -> s where
    -- | Replace the state inside the monad.
    put :: s -> m ()
    ...
```

So `put` is actually a method for class `MonadState` and may return any monad `m ()` as a result. In order to make put return what we want, we shall make any `MonadState` wrapped in `ReaderT` also an instance of `MonadState`.

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put s = lift $ put s

run' :: ReaderT Int (State String) ()
run' = do
    put "Hello"
    config <- ask
    case config of
        0 -> return ()
        n -> modify (++ replicate n 'a')
```

Before we move on and write a `MonadReader` for our `ReaderT`. I want to explain a little what does `| m -> s` means in the class definition. 

The `m -> s` is called a **functional dependency** in haskell. It tells `ghc` that if there is a `m` then there has and only has one `s` that makes `MonadState s m` being a valid instance. So `ghc` will not try to infer what `s` is once it knows what `m` is. This is like to tell `ghc` that I have a lemma that shows `m -> s`, and then `ghc` will try to prove and use this lemma when it checks your type. Otherwise in some situation when `s` can not be inferred, `ghc` will simply throw an error says it does not know what `s` is.

Ok, enough for theories, let us code. We need to move our `ask` into `MonadReader` so if someone is use `EitherT String (Reader String) ()`, they do not need to `lift` the `ask` as well.

```haskell
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    ask = reader id

    reader :: (r -> a) -> m a
    reader f = ask >>= \r -> return (f r)

-- Make `ReaderT` a `MonadReader`
instance Monad m => MonadReader r (ReaderT r m)
    ask = ReaderT return
```

For any monad transformer we want to stack on `ReaderT`, we may simply make it an instance of `MonadReader`. Then `ghc` will automatically `lift` them for us.
