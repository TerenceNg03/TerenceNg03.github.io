# C++的函數式特性

## 什麼是函數式編程？

函數式編程是一種編程範式，就像面向對象編程（OOP）一樣。在函數式編程中，我們專注於函數。函數可以是另一個函數的參數或返回值。有關更多信息，請參見[維基百科](https://en.wikipedia.org/wiki/Functional_programming)。

## Lambda 表達式

Lambda 表達式，也稱為匿名函數，是一種特殊類型的函數，它沒有名稱。當需要臨時函數時，它特別有用。如果您曾經使用過 Python，您可能熟悉以下 Python 代碼。

```python
lambda x: x if x>=0 else -x
```

這行代碼返回輸入變量 x 的絕對值。自從 C++11 以來，我們也可以這樣做。

```cpp
[=](double x)->int{return x>=0? x:-x ;};
```

### Lambda 表達式的語法

現在讓我們分析一下這行代碼的含義。

\[=\] 表示這個 lambda 捕獲所有外部變量的值。**請注意，默認情況下，所有被值捕獲的變量都是不可變的。** 所有可能的值包括：

- `[ ]`：不捕獲任何變量。
- `[=]`：通過值捕獲所有變量。
- `[&]`：通過引用捕獲所有變量。
- `[=,&y]`：通過引用捕獲 `y`，其它所有變量通過值捕獲。
- `[&,x]`：通過值捕獲 `x`，其它所有變量通過引用捕獲。
- `[&x,y]`：指定如何捕獲 `x` 和 `y`。不允許捕獲其他變量。

`(double)` 是應該傳遞給此 lambda 表達式的參數列表。**在 C++14 中，auto 關鍵字在此處也是允許的，這使得 lambda 函數具備了執行鴨子類型的能力。**

`->int` 指定此函數的返回類型為 `int`。**請注意，這不是自願的。如果聲明瞭函數，則必須返回 int。** 還允許其他一些選項。包括返回類型在內的所有選項都可以省略。

*   `mutable`: 允許修改被值捕獲的變量。
*   `noexcept`: 與普通函數使用方式相同。
*   `throw()`: 與普通函數使用方式相同。
*   `->int`: 特定返回類型。

### Lambda 表達式的使用

對於 lambda 表達式，它可以被存儲在變量或 `std::function` 中。或者它可以被立即調用，或直接傳遞給接受它作為參數的函數，例如 `std::sort`。

```cpp
/* 存儲在一個 auto 變量中 */
auto lambda1 = [](double x){return x>=0? 1:-1 ;};
/* 構建 std::function */
std::function lambda = 
    [](int x) { return x>=0? 1:-1 ; };
/* 直接調用 */
int i = [](double x){return x>=0? 1:-1 ;}(9);
/* 作為參數傳遞給函數 */
std::sort(v.begin(), v.end(), 
    [](auto x, auto y){return x>y;});
```

## Lambda 表達式作為參數傳遞 

在這裡，你有兩個選擇。你可以使用 `std::function` 或使用一個模板。

```cpp
/* std::function 解決方案 */
void f_(std::function lambda)
    {/* ... */};
/* 模板 */
template
void f(F lambda) { /* ... */}
```

## 遞歸的 Lambda Expression

通過將 lambda 表達式分配給 `std::function`，可以引用它本身。**請注意，函數必須被引用捕獲。否則它將被編譯，但會產生運行時錯誤。**

```cpp
std::function factorial = [=] (int i)
    {
        return (i == 1) ? 1 : i * factorial(i - 1);
    };
```

## 什麼是 std::function？

根據 [cppreference](https://en.cppreference.com/w/cpp/utility/functional/) 的說法，`std::function` 實例可以存儲、複製和調用 **任何可複製構造的可調用目標**。這包括 lambda 表達式、函數指針，甚至是成員函數。這裡是一個使用 `std::function` 而不是傳統函數指針的例子。

```cpp
int foo(){return 0;}
/* C 風格 */
int (*fp)(int) = foo;
/* std::function */
std::function func = foo;
```

## 在 C++ 中創建回調

如果你熟悉 Javascript，你可能已經知道回調的概念。

```javascript
function Callback() {
    console.log("I am executed!");
}
Callback();
```

簡而言之，回調是作為參數傳遞給另一個函數的函數，直到回調被調用該函數才執行。在引入 `std::function` 之前，這是通過接口和抽象類實現的，有時可能是一個不乾淨的解決方案，會造成很多混亂。例如，我們可以創建一個類方法的回調，而不使用接口。

```cpp
class executer{
public:
    int exec(std::function f){
        return f(1);
    }
};

int foo(int a){return a;}

class bar{
public:
    int run(int a){return a;}
};

int main(){
    executer e;
    bar s;
    e.exec(foo);
    using std::placeholders::_1;
    e.exec(std::bind(&bar::run, &s, _1));
}
```

在上面的例子中，如果不使用回調，我們仍然可以通過傳遞函數指針來運行 `foo`。然而，要執行 `bar::run`，我們將不得不繼承 `executer`，以便它將 `bar` 作為參數接受，因為非靜態成員函數無法被引用。

## std::bind 的用法

`std::bind` 的作用是綁定參數和函數以創建一個新的函數對象。它還可以僅綁定部分參數列表，甚至使用 `std::placeholders` 重新排序參數。

```cpp
int foo(int a, int b){return a-b;}

class Bar{
public:
    int run(int a){return a;}
};

int main(){
    /* _1 _2 ... _N 來自這裡 */
    using namespace std::placeholders;
    /* foo(b, a) */
    auto f1 = std::bind(foo,_2,_1);
    /* foo(a, 3) */
    auto f2 = std::bind(foo,_1, 3);    
    /* foo(1, 3) */
    auto f3 = std::bind(foo, 1, 3);
    /* 返回 bar.run(a) */
    Bar b;
    auto f4 = std::bind(&Bar::run, &b, _1);
    /* 打印 1 0 -2 6 */
    printf("%d %d %d %d",
           f1(1,2), f2(3,1), f3(-1,-2, 5), f4(6));
}
```

`std::bind` 接受佔位符和實際對象。如果它將佔位符綁定到函數，則必須在調用生成的函數時傳遞參數。任何其他參數都將被忽略。也可以將成員函數與實際的類對象綁定。
