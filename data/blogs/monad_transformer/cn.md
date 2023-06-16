# Monad Transformers 簡介

在我們開始之前，我想用一個例子來說明為什麼我們需要引入 monad transformer 以及它試圖解決什麼問題。

## Monad Stack

有時我們有一些基本的 monad，比如 `IO`，`Maybe` 和 `State`。每個 monad 都處理自己的事情。但是，如果我想在 `State` monad 中返回可能不存在的東西，該怎麼辦？直覺上，我們會寫出這樣的代碼。

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

但問題是每次我們嘗試獲取狀態時，都必須進行模式匹配。這段代碼是純粹的樣板代碼，因為我們已經有了 `Maybe` monad 來只在有東西時繼續計算。我們想要的是一些可以將不同 monad 的能力結合在一起的東西。這就是我今天要談論的 monad transformer。

## Monad Transformer 是什麼

Monad Transformer 是一個同時是 `Monad` 和 `MonadTrans` 類的東西。讓我們先看一下它們的類型簽名。

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

class MonadTrans (t :: (* -> *) -> * -> *) where
    lift :: Monad m => m a -> t m a
```

你可能會看到 `MonadTrans` 接受一個 `* -> *` 和一個 `*`，然後給我們一個真正的 `Type`。它只有一個非常有趣的方法叫做 `lift`。它接受一個 monad `m`（對應於 `* -> *`），然後將其包裝到自身（`t`）中，以便我們得到一個 `t m a`。

通常，一個 Monad Transformer 也是一個 monad。這意味著我們可以這樣做：

```haskell
run = do 
    config <- ask
    lift $ print config
```

這段代碼將 `Reader` 轉換為 `IO` monad，讓我們輕鬆地獲取 monad 中的內容。太棒了！

## 實現 ReaderT

`ReaderT` 是一個非常簡單的 monad transformer。它接受一個**只讀配置**並沿著 monad 傳遞，同時允許您使用 `ask` 查詢配置。

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

我們需要實現一些東西來獲取我們的 `r`。

```haskell
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
```

有時我們只想使用單個 `Reader`，因此讓我們將 `Reader` 作為類型別名，將 `ReaderT` 應用於 `Identity` monad。

```haskell
import Control.Monad.Identity (Identity)
type Reader r = ReaderT r Identity
```

## 使用 ReaderT

現在我們已經定義了 `ReaderT`，是時候試驗一下它了。

```haskell
run :: ReaderT Int (State String) ()
run = do
    lift $ put "Hello"
    config <- ask
    case config of
        0 -> return ()
        n -> lift $ modify (++ replicate n 'a')
```

正如你所看到的，我們可以在 do 表達式塊中任何地方讀取我們的配置。但代價是我們必須在其他所有地方都放置一個 lift，這幾乎使我們的 `ReaderT` 不可用。有沒有辦法改進這一點呢？

## MonadX 類

最明顯的解決方案是給 `lift put` 做一個別名。

```haskell
putLifted = lift put
putLiftedLifted = lift putLifted
putLiftedLiftedLifted = lift putLiftedLifted
```

相當繁瑣，對吧？

讓我們看看 `put` 究竟是什麼。

```haskell
class Monad m => MonadState s m | m -> s where
    -- | Replace the state inside the monad.
    put :: s -> m ()
    ...
```

所以，`put` 實際上是類 `MonadState` 的一個方法，可以返回任何 `m ()` 類型的單子。為了使 `put` 返回我們想要的內容，我們應該使任何被 `ReaderT` 包裝的 `MonadState` 也成為 `MonadState` 的一個實例。

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
在我們繼續編寫 `ReaderT` 的 `MonadReader` 之前，我想簡要解釋一下類定義中的 `| m -> s` 是什麼意思。

在 Haskell 中，`m -> s` 被稱為 **函數依賴（functional dependency）**。它告訴 `ghc`，如果有一個 `m`，那麼就有且僅有一個 `s` 使得 `MonadState s m` 成為一個有效的實例。因此，一旦 `ghc` 知道了 `m` 是什麼，它就不會嘗試推斷 `s` 是什麼。這就像告訴 `ghc`，我有一個引理表明 `m -> s`，然後當它檢查你的類型時，`ghc` 將嘗試證明並使用這個引理。否則，在某些情況下，當 `s` 無法推斷出來時，`ghc` 將簡單地拋出一個錯誤，表示它不知道 `s` 是什麼。

好了，理論夠了，我們來寫代碼吧。我們需要將我們的 `ask` 移入 `MonadReader` 中，這樣如果有人使用 `EitherT String (Reader String) ()`，他們也不需要 `lift ask`這種代碼了。

```haskell
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Monad m => MonadReader r m | m -> r where
    ask :: m r
    ask = reader id

    reader :: (r -> a) -> m a
    reader f = ask >>= \r -> return (f r)

-- 將 `ReaderT` 變為 `MonadReader` 的實例
instance Monad m => MonadReader r (ReaderT r m)
```

對於我們想要堆疊在 `ReaderT` 上的任何單子變換器，我們只需要將其變為 `MonadReader` 的一個實例。然後 `ghc` 就會自動為我們 `lift` 它們。
