# 來寫一個Parser Combinator

在之前的文章中，我已經談到了使用 `flex & bison` 生成解析器。然而，這樣的解析器已知存在一系列問題。首先是它們幾乎無法進行調試和測試。在實踐中，我們希望以小單位測試我們的解析器，從而可以精確定位錯誤。另一個常見問題是讓解析器生成器生成代碼會使編譯更加複雜（特別是對於像 `C++` 這樣的語言），用戶被迫學習它們自己的 API，這可能非常繁瑣。

## 那麼，什麼是Parser Combinator？

Parser Combinator是一個 `Monad`，同時也是 `Functor` 和 `Applicative` 的instance。它允許我們將小型和簡單的解析器組合成大型和複雜的解析器，並且不需要編寫任何語法/語法文件。例如，為了使用解析器組合器識別標識符，我們可以這樣做：

```haskell
charP :: (Char -> Bool) -> Parser Char
charP = undefined -- detail omitted here

repeatP :: Parser a -> Parser [a]
repeatP = undefined -- detail omitted here

identP :: Parser String
identP = liftA2 (:) startP $ repeatP bodyP
    where bodyP = charP $ not . isSpace
          startP = charP isLetter
```

## 好的，告訴我怎麼做

像大多數 `Monad` 一樣，讓我們從將 `Parser` 定義為 **封裝的 lambda 函數** 開始，它將 `String` 轉換為剩餘的字符串和解析的數據（類型為 `a`）的元組。

這裡我們將編寫一個簡單的，不做任何錯誤處理的`BaseParser`，之後再填加上相關的方法

```haskell
newtype BaseParser a = BaseParser 
    {runBaseParser :: String -> (String, a)}
```

現在讓我們將其變為 `Functor`。

```haskell
instance Functor BaseParser where
    fmap f p = BaseParser $ \s ->
        let (s', a) = runBaseParser p s
         in (s', f a)
```

由於`Functor`的特性，現在我們能夠將 `BaseParser a` 轉換為 `BaseParser b`，但我們仍然無法組合兩個解析器，而這是解析上下文無關文法（CFG）所需的。這就是為什麼我們必須將其變為 `Applicative`。儘管 `Applicative` 足以處理任何上下文無關文法（CFG），但我們可能對更強大的內容感興趣，以便解析上下文敏感文法（CSG）。這要求我們將我們的 `Parser` 變成一個 `Monad`。

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

大功告成，我們用了20多行代碼就搞定了這個`BaseParser`。

## 錯誤處理？

`BaseParser` 定義沒有提到錯誤處理，但這並不意味著`BaseParser`不能處理錯誤。只需要加上`ExceptT`這一Monad Transformer然後包裝成一個新類型即可。

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Except (ExceptT)

newtype Parser a = Parser (ExceptT String BaseParser a)
    deriving (Functor, Applicative, Monad)
```

這就是我們需要的所有代碼。

## 組合「或」解析器

`Applicative` 和 `Monad` 允許我們將幾個解析器組合在一起，以便解析一個序列。但是"垂直"的組合呢？比如解析 `json` 格式的文本，我們可能想嘗試一起解析一個**數字**或**字符串**或**數組**。為了滿足這種要求，我們將為 `Parser` 引入一個新運算符`<|>`。

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

## 一些常用的Parser實現

現在我們的解析器已經準備好解析任何東西了。我在這裡給出一些示例，同時也可以參考 [我的 Seml Github 倉庫](https://github.com/TerenceNg03/Seml/)，實現了一個將 S 表達式解析為 XML的Parser。

### Char 解析器

從一個只能解析一個字符的解析器開始。

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

### 重複Parser

現在我們想解析一個標識符。為此，我們需要重復 `charP` `n` 次。

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

請注意，即使我們在這裡處理了空字符串，**重復接受空字符串的Parser**仍會導致**無限循環**。

### between Parser

好的，我們可以解析一個標識符。但是解析帶引號的字符串怎麼辦？

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
```

### 分隔符Parser

假設有一個由一些隨機空格分隔的單詞列表。我們該如何解析這些東西呢？

```haskell
-- | Repeat parser with separator 
sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = liftA2 (:) pa
```
