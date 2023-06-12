# 來写一个Parser Combinator

在之前的文章中，我已经谈到了使用 `flex & bison` 生成解析器。然而，这样的解析器已知存在一系列问题。首先是它们几乎无法进行调试和测试。在实践中，我们希望以小单位测试我们的解析器，从而可以精确定位错误。另一个常见问题是让解析器生成器生成代码会使编译更加复杂（特别是对于像 `C++` 这样的语言），用户被迫学习它们自己的 API，这可能非常繁琐。

## 那么，什么是Parser Combinator？

Parser Combinator是一个 `Monad`，同時也是 `Functor` 和 `Applicative` 的instance。它允许我们将小型和简单的解析器组合成大型和复杂的解析器，并且不需要编写任何语法/语法文件。例如，为了使用解析器组合器识别标识符，我们可以这样做：

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

## 好的，告诉我怎么做

像大多数 `Monad` 一样，让我们从将 `Parser` 定义为 **封装的 lambda 函数** 开始，它将 `String` 转换为剩余的字符串和解析的数据（类型为 `a`）的元组。

这里我们将编写一个简单的，不做任何错误處理的`BaseParser`，之後再填加上相關的方法

```haskell
newtype BaseParser a = BaseParser 
    {runBaseParser :: String -> (String, a)}
```

现在让我们将其变为 `Functor`。

```haskell
instance Functor BaseParser where
    fmap f p = BaseParser $ \s ->
        let (s', a) = runBaseParser p s
         in (s', f a)
```

由於`Functor`的特性，现在我们能够将 `BaseParser a` 转换为 `BaseParser b`，但我们仍然无法组合两个解析器，而这是解析上下文无关文法（CFG）所需的。这就是为什么我们必须将其变为 `Applicative`。尽管 `Applicative` 足以处理任何上下文无关文法（CFG），但我们可能对更强大的内容感兴趣，以便解析上下文敏感文法（CSG）。这要求我们将我们的 `Parser` 变成一个 `Monad`。

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

大功告成，我們用了20多行代碼就搞定了这個`BaseParser`。

## 错误处理？

`BaseParser` 定义没有提到错误处理，但这并不意味着`BaseParser`不能处理错误。只需要加上`ExceptT`這一Monad Transformer然後包装成一個新類型即可。

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Except (ExceptT)

newtype Parser a = Parser (ExceptT String BaseParser a)
    deriving (Functor, Applicative, Monad)
```

这就是我们需要的所有代码。

## 组合“或”解析器

`Applicative` 和 `Monad` 允许我们将几个解析器组合在一起，以便解析一个序列。但是"垂直"的组合呢？比如解析 `json` 格式的文本，我们可能想尝试一起解析一个**数字**或**字符串**或**数组**。为了满足这种要求，我们将为 `Parser` 引入一个新运算符`<|>`。

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

现在我们的解析器已经准备好解析任何东西了。我在这里给出一些示例，同時也可以参考 [我的 Seml Github 仓库](https://github.com/TerenceNg03/Seml/)，實現了一個将 S 表达式解析为 XML的Parser。

### Char 解析器

从一个只能解析一个字符的解析器开始。

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

现在我们想解析一个标识符。为此，我们需要重复 `charP` `n` 次。

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

请注意，即使我们在这里处理了空字符串，**重复接受空字符串的Parser**仍会导致**无限循环**。

### between Parser

好的，我们可以解析一个标识符。但是解析带引号的字符串怎么办？

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

假设有一个由一些随机空格分隔的单词列表。我们该如何解析这些东西呢？

```haskell
-- | Repeat parser with separator 
sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = liftA2 (:) pa
```
