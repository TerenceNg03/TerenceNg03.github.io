# C++的函數式特性

## 什么是函数式编程？

函数式编程是一种编程范式，就像面向对象编程（OOP）一样。在函数式编程中，我们专注于函数。函数可以是另一个函数的参数或返回值。有关更多信息，请参见[维基百科](https://en.wikipedia.org/wiki/Functional_programming)。

## Lambda 表达式

Lambda 表达式，也称为匿名函数，是一种特殊类型的函数，它没有名称。当需要临时函数时，它特别有用。如果您曾经使用过 Python，您可能熟悉以下 Python 代码。

```python
lambda x: x if x>=0 else -x
```

这行代码返回输入变量 x 的绝对值。自从 C++11 以来，我们也可以这样做。

```cpp
[=](double x)->int{return x>=0? x:-x ;};
```

### Lambda 表达式的语法

现在让我们分析一下这行代码的含义。

\[=\] 表示这个 lambda 捕获所有外部变量的值。**请注意，默认情况下，所有被值捕获的变量都是不可变的。** 所有可能的值包括：

- `[ ]`：不捕获任何变量。
- `[=]`：通过值捕获所有变量。
- `[&]`：通过引用捕获所有变量。
- `[=,&y]`：通过引用捕获 `y`，其它所有变量通过值捕获。
- `[&,x]`：通过值捕获 `x`，其它所有变量通过引用捕获。
- `[&x,y]`：指定如何捕获 `x` 和 `y`。不允许捕获其他变量。

`(double)` 是应该传递给此 lambda 表达式的参数列表。**在 C++14 中，auto 关键字在此处也是允许的，这使得 lambda 函数具备了执行鸭子类型的能力。**

`->int` 指定此函数的返回类型为 `int`。**请注意，这不是自愿的。如果声明了函数，则必须返回 int。** 还允许其他一些选项。包括返回类型在内的所有选项都可以省略。

*   `mutable`: 允许修改被值捕获的变量。
*   `noexcept`: 与普通函数使用方式相同。
*   `throw()`: 与普通函数使用方式相同。
*   `->int`: 特定返回类型。

### Lambda 表達式的使用

对于 lambda 表达式，它可以被存储在变量或 `std::function` 中。或者它可以被立即调用，或直接传递给接受它作为参数的函数，例如 `std::sort`。

```cpp
/* 存储在一个 auto 变量中 */
auto lambda1 = [](double x){return x>=0? 1:-1 ;};
/* 构建 std::function */
std::function lambda = 
    [](int x) { return x>=0? 1:-1 ; };
/* 直接调用 */
int i = [](double x){return x>=0? 1:-1 ;}(9);
/* 作为参数传递给函数 */
std::sort(v.begin(), v.end(), 
    [](auto x, auto y){return x>y;});
```

## Lambda 表達式作為參數傳递 

在这里，你有两个选择。你可以使用 `std::function` 或使用一个模板。

```cpp
/* std::function 解决方案 */
void f_(std::function lambda)
    {/* ... */};
/* 模板 */
template
void f(F lambda) { /* ... */}
```

## 递歸的 Lambda Expression

通过将 lambda 表达式分配给 `std::function`，可以引用它本身。**请注意，函数必须被引用捕获。否则它将被编译，但会产生运行时错误。**

```cpp
std::function factorial = [=] (int i)
    {
        return (i == 1) ? 1 : i * factorial(i - 1);
    };
```

## 什么是 std::function？

根据 [cppreference](https://en.cppreference.com/w/cpp/utility/functional/) 的说法，`std::function` 实例可以存储、复制和调用 **任何可复制构造的可调用目标**。这包括 lambda 表达式、函数指针，甚至是成员函数。这里是一个使用 `std::function` 而不是传统函数指针的例子。

```cpp
int foo(){return 0;}
/* C 风格 */
int (*fp)(int) = foo;
/* std::function */
std::function func = foo;
```

## 在 C++ 中创建回调

如果你熟悉 Javascript，你可能已经知道回调的概念。

```javascript
function Callback() {
    console.log("I am executed!");
}
Callback();
```

简而言之，回调是作为参数传递给另一个函数的函数，直到回调被调用该函数才执行。在引入 `std::function` 之前，这是通过接口和抽象类实现的，有时可能是一个不干净的解决方案，会造成很多混乱。例如，我们可以创建一个类方法的回调，而不使用接口。

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

在上面的例子中，如果不使用回调，我们仍然可以通过传递函数指针来运行 `foo`。然而，要执行 `bar::run`，我们将不得不继承 `executer`，以便它将 `bar` 作为参数接受，因为非静态成员函数无法被引用。

## std::bind 的用法

`std::bind` 的作用是绑定参数和函数以创建一个新的函数对象。它还可以仅绑定部分参数列表，甚至使用 `std::placeholders` 重新排序参数。

```cpp
int foo(int a, int b){return a-b;}

class Bar{
public:
    int run(int a){return a;}
};

int main(){
    /* _1 _2 ... _N 来自这里 */
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

`std::bind` 接受占位符和实际对象。如果它将占位符绑定到函数，则必须在调用生成的函数时传递参数。任何其他参数都将被忽略。也可以将成员函数与实际的类对象绑定。
