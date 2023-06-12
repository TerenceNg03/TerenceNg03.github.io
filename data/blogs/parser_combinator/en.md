# Write me a Parser Combinator

In previous articles I have talked about using `flex & bison` to generate a parser. However, such parsers are known to suffer from a series of problems. The first is that they are almost impossible to debug and test. In practice, we want to test our parser in small units and thus can locate bugs precisely. Another common issue is let parser generator to generate code makes compiling more complex (especially for languages like `C++`) and the user is forced to learn their own APIs which can be very tedious.

## So, What is a parser combinator?

Parser combinator is a `Monad` in a nut shell, which also implies it is an instance of `Functor` and `Applicative`. It allows us to combine small and simple parsers into large and complex parsers and does not require to write any syntax/grammar files. For example, in order to recognize an identifier with parser combinators, we may do:

```haskell
charP :: (Char -> Bool) -> Parser Char
charP = undefined -- detail omitted here

repeatP :: Parser a -> Parser [a]
repeatP = undefined --detail omitted here

identP :: Parser String
identP = liftA2 (:) startP $ repeatP bodyP
    where bodyP = charP $ not . isSpace
          startP = charP isLetter
```

## Ok, Show me how to do it.

Like most `Monad`s, let us start by defining a `Parser` as **an wrapped lambda function** that turns a `String` into a tuple of the remained string and parsed data (of type `a`).

Note: we will write a simple `BaseParser` that does not handle errors at all. The real **`Parser`** will be defined later.

```haskell
newtype BaseParser a = BaseParser 
    {runBaseParser :: String -> (String, a)}
```

Now let us make it a `Functor`.

```haskell
instance Functor BaseParser where
    fmap f p = BaseParser $ \s ->
        let (s', a) = runBaseParser p s
         in (s', f a)
```

As you may see, we are now able to transform `BaseParser a` into `BaseParser b`, yet we still can not combine two parser which is required to parser context free grammar (CFG). And that is why we must make it an `Applicative`. Note that although `Applicative` is enough to handle any context free grammar (CFG), we may be interested in something more powerful in order to parse context sensitive grammar (CSG). This requires us to make our `Parser` a `Monad`.

```haskell
{-# LANGUAGE TupleSections #-}
import Control.Monad (ap)

instance Applicative BaseParser where
    pure a = BaseParser (,a)
    (<*>) = ap

instance Monad BaseParser where
    return = pure
    p >>= f = BaseParser $ \s ->
        let (s', a) = runBaseParser p s
         in runBaseParser (f a) s'
```

And that is it, you now have a working parser combinator with about 20 lines of code.

## But What about Error Handling?

Our `BaseParser` definition says nothing about error handling, but it doesn't means we can not handle errors outside `BaseParser` with a **monad transformer**. 

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Except (ExceptT)

newtype Parser a = Parser (ExceptT String BaseParser a)
    deriving (Functor, Applicative, Monad)
```

And that is all the codes we need.

## Combining "or" parsers

`Applicative` and `Monad` allows us to combine several parser together so that we can parse a sequence. But what about "vertically" combine? If we are parsing something like `json`, we probably want to try to parse a **number** or a **string** or an **array** together. For such requirement, we would introduce a new operator for `Parser`.

```haskell
import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Except (runExceptT)

parser :: (String -> (String, Either String a)) -> Parser a
parser = Parser . ExceptT . BaseParser

runParser :: Parser a -> String -> (String, Either String a)
runParser (Parser p)= runBaseParser . runExceptT $ p

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = parser $ \s ->
    case runParser p1 s of
        (_, Left _) -> runParser p2 s
        a -> a
```

## Some commonly used parser

Now our parser is ready to parse anything. I here by gives some examples to illustrate how to use it. You can also check out [my Seml Github Repo](https://github.com/TerenceNg03/Seml/) which parse S-Expressions into XML.

### Char Parser

Start with a parser that can parse only one character.

```haskell
charP :: (Char -> Bool) -> Parser Char
charP f = parser $ \s ->
    case s of
        (x : xs)
            | f x -> (xs, Right x)
            | otherwise -> (s, Left "Invalid Char"")
        [] -> (s, Left "Eof Error")

isP :: Char -> Parser Char
isP c = charP (== c)
```

### Repeat Parser
Now we would like to parse an identifier. To do this we need to repeat `charP` for `n` times.

```haskell
import Control.Applicative (liftA2)
import Data.Char (isAlphaNum, isLetter)

someP :: Parser a -> Parser [a]
someP p = liftA2 (:) p (manyP p)

-- Be careful here we do not want to stuck
-- on an empty string
manyP :: Parser a -> Parser [a]
manyP p = terminateP <|> someP p <|> return []

terminateP :: Parser [a]
terminateP = parser $ \s ->
    case s of
        [] -> ([], Right [])
        _ -> (s, Left "")

-- | Repeat a parser
repeatP :: Parser a -> Parser [a]
repeatP = manyP

-- | Recognize an identifier
identP :: Parser String
identP = liftA2 (:) (charP isLetter) $
    repeatP $ charP isAlphaNum
```

Note that even though we have handled empty string here, **repeat a parser that accepts an empty string** will still cause **an infinite loop**. Be aware!
### Between Parser
Ok, we can parse an identifier. But what about parse a quoted string?

```haskell
-- | Parse something between other things
betweenP :: Parser l -> Parser r -> Parser mid -> Parser mid
betweenP l r m = l *> m <* r

-- Simplified escape handling, we just return what ever 
-- character that is escaped and do not translate 
-- `\n` into newline or etc.
escapeSP :: Parser Char
escapeSP = isP '\\' *> charP (const True)

quotedP :: Parser String
quotedP =
    let ssP = repeatP (escapeSP <|> charP (/= '\''))
        dsP = repeatP (escapeSP <|> charP (/= '"'))
        singleP = betweenP (isP '\'') (isP '\'') ssP
        doubleP = betweenP (isP '"') (isP '"') dsP
     in singleP <|> doubleP
```

### Separated by Parser

Say we have a list of words separated by some random spaces. How do we parse such things?

```haskell
-- | Repeat parser with separator
sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = liftA2 (:) pa (repeatP (ps *> pa))

spaceP :: Parser String
spaceP = repeatP (charP isSpace)

wordsP :: Parser [String]
wordsP = sepByP spaceP identP
```



