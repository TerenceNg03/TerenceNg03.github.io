# Flex & Bison生成C++代碼教學 

## 什麼是 Flex & Bison？

Flex & Bison 是 GNU 開發的一套工具，用於生成可以處理結構化輸入的解析器。Flex 是一個分詞器，將輸入字符流分成標記。這些標記然後被發送到 Bison，它將根據特定的語法規則匹配它們，並相應地執行操作。

## Flex & Bison 和 C++

Flex & Bison 已經存在了超過 40 年。當它們剛出生時，甚至沒有 C++。因此，它們最初只生成 C 中的解析器，其中包含許多全局變量，通常無法重用。不幸的是，即使它們現在正式支持 C++，大多數教程仍然使用 C 代碼。這就是為什麼我想做這個教程來幫助你切換到 C++。在這裡，我假設您已經掌握了基本用法，如果您沒有，請參考[這篇文章](http://www.capsl.udel.edu/courses/cpeg421/2012/slides/Tutorial-Flex_Bison.pdf)。

## 設置驅動Driver

我們的解析器的大綱將如下所示。  

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
    location*    location;// 用於跟蹤錯誤
};
```

## 設置Scanner

在這一部分中，我們將設置我們的 Flex 文件 `scanner.l`。

```cpp
%option nodefault
%option debug
%option noyywrap
%option prefix="MyParser"
%option yylineno
%option c++
```

首先，我們需要將上述選項放入我們的 Flex 文件中。它們的含義如下：

* `nodefault`：讓 Flex 不生成默認標記。
* `debug`：啓用調試信息。
* `noyywrap`：即使讀取到 EOF，Flex 也會繼續運行。
* `prefix`：生成的詞法分析器的特定命名空間。
* `yylineno`：計算行號。
* `c++`：要求 Flex 生成 C++ 詞法分析器。

在此之後，我們可以定義我們的規則，如下所示。

```cpp
[0-9]{1,} return MyParser::Parser::make_NUM(atoi(yytext),loc);
"-" return MyParser::Parser::make_MINUS(loc);
"+" return MyParser::Parser::make_PLUS(loc);
\n return MyParser::Parser::make_NEWLINE(loc);
[ \t]+ /* ignore whitespace */
. return MyParser::Parser::make_ILLEGAL(std::string(yytext),loc);
```

## 設置Parser

這些選項需要放入 `parser.y` 中。

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

它們的含義如下：

* `location`：啓用位置跟蹤。
* `parse.error verbose`：讓 Bison 生成詳細的錯誤消息。
* `parse-param`：傳遞給 yyparse 的參數。
* `lex-param`：傳遞給 yylex 的參數。
* `language`：啓用位置跟蹤。
* `variant`：使用 C++ variant 特性而不是 C 風格的聯合。
* `constructor`：生成類似於我們在 `scanner.l` 中使用的 `make_PLUS` 的構造函數。

然後，我們可以添加token和type。

```plaintext
%token NEWLINE PLUS MINUS 
%token  NUM 
%token END
%token  ILLEGAL
%type  EXPR    
```

我們可以為每個語法規則返回值類型。token的類型必須與 `scanner.l` 指定的規則匹配。

## 錯誤檢測和恢復

為了逐行讀取輸入，以便我們可以正確顯示錯誤消息。我們需要重新加載 `LexerInput`。

cpp
virtual size_t LexerInput( char* buf, size_t max_size );

        

並且在讀取令牌或換行符到達時更新位置。

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

        

現在我們已經跟蹤了位置並緩衝了每行輸入。當出現錯誤時，Bison 將調用 `Parser::error()` 並停止解析。為了防止這種情況發生，我們可以利用一個特殊的規則 `error`，它將匹配所有未識別的標記。

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

錯誤消息將會是這樣的。

```plaintext
Enter expression:12+6-0&+66
line 1.1-7: syntax error, unexpected ILLEGAL, expecting NEWLINE
    12+6-0&+66
    ~~~~~~^
Enter expression:
```

## 代碼下載

更多細節和完整的代碼，你可以查看我的 [Github 倉庫](https://github.com/TerenceNg03/Flex-Bison-Full-Cpp-Example)。

