# Flex & Bison生成C++代碼教學 

## 什么是 Flex & Bison？

Flex & Bison 是 GNU 开发的一套工具，用于生成可以处理结构化输入的解析器。Flex 是一个分词器，将输入字符流分成标记。这些标记然后被发送到 Bison，它将根据特定的语法规则匹配它们，并相应地执行操作。

## Flex & Bison 和 C++

Flex & Bison 已经存在了超过 40 年。当它们刚出生时，甚至没有 C++。因此，它们最初只生成 C 中的解析器，其中包含许多全局变量，通常无法重用。不幸的是，即使它们现在正式支持 C++，大多数教程仍然使用 C 代码。这就是为什么我想做这个教程来帮助你切换到 C++。在这里，我假设您已经掌握了基本用法，如果您没有，请参考[这篇文章](http://www.capsl.udel.edu/courses/cpeg421/2012/slides/Tutorial-Flex_Bison.pdf)。

## 设置驱动Driver

我们的解析器的大纲将如下所示。  

```cpp
class Driver
{
public:
    Driver();
    ~Driver();

    int parse();
    int parse_file(std::string& path);

private:
    Scanner*    scanner; // 由 Flex 生成
    Parser*        parser;  // 由 Bison 生成
    location*    location;// 用于跟踪错误
};
```

## 设置Scanner

在这一部分中，我们将设置我们的 Flex 文件 `scanner.l`。

```cpp
%option nodefault
%option debug
%option noyywrap
%option prefix="MyParser"
%option yylineno
%option c++
```

首先，我们需要将上述选项放入我们的 Flex 文件中。它们的含义如下：

* `nodefault`：让 Flex 不生成默认标记。
* `debug`：启用调试信息。
* `noyywrap`：即使读取到 EOF，Flex 也会继续运行。
* `prefix`：生成的词法分析器的特定命名空间。
* `yylineno`：计算行号。
* `c++`：要求 Flex 生成 C++ 词法分析器。

在此之后，我们可以定义我们的规则，如下所示。

```cpp
[0-9]{1,} return MyParser::Parser::make_NUM(atoi(yytext),loc);
"-" return MyParser::Parser::make_MINUS(loc);
"+" return MyParser::Parser::make_PLUS(loc);
\n return MyParser::Parser::make_NEWLINE(loc);
[ \t]+ /* ignore whitespace */
. return MyParser::Parser::make_ILLEGAL(std::string(yytext),loc);
```

## 设置Parser

这些选项需要放入 `parser.y` 中。

```plaintext
%locations
%define api.namespace {MyParser}
%define api.parser.class {Parser}
%lex-param {MyParser::Driver &driver}
%parse-param {MyParser::Driver &driver}
%define parse.error verbose
%language "c++"
%define api.value.type variant
%define api.token.constructor
```

它们的含义如下：

* `location`：启用位置跟踪。
* `parse.error verbose`：让 Bison 生成详细的错误消息。
* `parse-param`：传递给 yyparse 的参数。
* `lex-param`：传递给 yylex 的参数。
* `language`：启用位置跟踪。
* `variant`：使用 C++ variant 特性而不是 C 风格的联合。
* `constructor`：生成类似于我们在 `scanner.l` 中使用的 `make_PLUS` 的构造函数。

然后，我们可以添加token和type。

```plaintext
%token NEWLINE PLUS MINUS 
%token  NUM 
%token END
%token  ILLEGAL
%type  EXPR    
```

我们可以为每个语法规则返回值类型。token的类型必须与 `scanner.l` 指定的规则匹配。

## 错误检测和恢复

为了逐行读取输入，以便我们可以正确显示错误消息。我们需要重新加载 `LexerInput`。

cpp
virtual size_t LexerInput( char* buf, size_t max_size );

        

并且在读取令牌或换行符到达时更新位置。

cpp
/* scanner.l */
#define YY_USER_ACTION \
{loc.columns(yyleng); \
driver.scanner->current_col = \
    driver.scanner->current_col_end; \
driver.scanner->current_col_end += yyleng;}

/* parser.y */
STATEMENT : 
{  
    printf("Enter expression:");
}
| STATEMENT EXPR NEWLINE
{
    printf("The result is %f\n",$2);
    printf("Enter expression:");
    driver.location->lines();
    driver.location->step();
    driver.scanner->reset_current_col();
}

        

现在我们已经跟踪了位置并缓冲了每行输入。当出现错误时，Bison 将调用 `Parser::error()` 并停止解析。为了防止这种情况发生，我们可以利用一个特殊的规则 `error`，它将匹配所有未识别的标记。

```cpp
/* Error display */
void Parser::error
    (const location& loc, const std::string& m)
{
    size_t current_col 
        = driver.scanner->current_col;
    std::cout<< "line " <<loc<< ": " <<m<< "\n";
    fprintf(stderr,"\t%s\t",
        driver.scanner->current_line.c_str());
    for(int i = 0; i < current_col; i++)
        fprintf(stderr,"~");
    fprintf(stderr,"^\n");
    }

/* Error recovery */
| STATEMENT error NEWLINE
{
    driver.location->lines();
    driver.location->step();
    driver.scanner->reset_current_col();
    printf("Enter expression:");
}
```

错误消息将会是这样的。

```plaintext
Enter expression:12+6-0&+66
line 1.1-7: syntax error, unexpected ILLEGAL, expecting NEWLINE
    12+6-0&+66
    ~~~~~~^
Enter expression:
```

## 代码下载

更多细节和完整的代码，你可以查看我的 [Github 仓库](https://github.com/TerenceNg03/Flex-Bison-Full-Cpp-Example)。

