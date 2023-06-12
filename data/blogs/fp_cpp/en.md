# Functional Programming in C++

## What is functional programming?

Functional programming is a programming paradigm like Object-Oriented Programming(OOP). In functional programming we focus on functions. Functions can be a parameter or return value of another function. See [Wikipedia](https://en.wikipedia.org/wiki/Functional_programming) for more information.

## Lambda Expression

Lambda AKA anonymous function is a special kind of function that does not have a name. It is especially useful when a temporary function is needed. If you have ever used python, you may be familiar with this python code.

```python
lambda x: x if x>=0 else -x
```

This line of code return the absolute value of input variable x. Since C++11, we can do this.

```cpp
[=](double x)->int{return x>=0? x:-x ;};
```

### Lambda Expression - Grammar

Now let's analyze what does this line of code means.

\[=\] means that this lambda capture all outside variables by value. **Note that all variables that are captured by value is immutable by default.** All possible values here are:

*   `[ ]`: Do not capture any variable.
*   `[=]`: Capture all variables by value.
*   `[&]`: Capture all variables by reference.
*   `[=,&y]`: Capture `y` by reference and any other one by value.
*   `[&,x]`: Capture `x` by value and any other ones by reference.
*   `[&x,y]`: Specific how to capture `x` and `y`. Other variables are not allowed to be captured.

`(double)` is the list of parameters that should be passed to this lambda expression. **In C++14, auto keyword is allowed here, which gives lambda function the ability to perform duck typing.**

`->int` specific that this function return type is `int`. **Note that this is not voluntary. The function must return an int if declared.** Some other options are also allowed here. All the options including return type can be omitted.

*   `mutable`: Allow modification performed on variables that are captured by value.
*   `noexcept`: The same how it is used in a normal function.
*   `throw()`: The same how it is used in a normal function.
*   `->int`: Specific return type.

### Lambda Expression - Usage

For lambda expressions, it can be stored in an variable or `std::function`. Or it can be called instantly or passed directly to a function that accepted it as parameter, `std::sort` for instance.

```cpp
/* Save in an auto variable */
auto lambda1 = [](double x){return x>=0? 1:-1 ;};
/* Construct std::function */
std::function lambda = 
    [](int x) { return x>=0? 1:-1 ; };
/* Directly called */
int i = [](double x){return x>=0? 1:-1 ;}(9);
/* Pass to function as parameter */
std::sort(v.begin(), v.end(), 
    [](auto x, auto y){return x>y;});
```

## Lambda expression as a parameter

Here you have two choices. You may either use `std::function` or use a template.

```cpp
/* std::function solution */
void f_(std::function lambda)
    {/* ... */};
/* Template */
template
void f(F lambda) { /* ... */}
```

## Recursive Lambda Expression

By assigning lambda expression to `std::function`, it is possible to reference itself. **Note that the function must be captured by reference. Otherwise it will compile but give a run-time error.**

```cpp
std::function factorial = [=] (int i)
    {
        return (i == 1) ? 1 : i * factorial(i - 1);
    };
```

## What is std::function?

According to [cppreference](https://en.cppreference.com/w/cpp/utility/functional/), instances of `std::function` can store, copy, and invoke **any CopyConstructible Callable** target. This includes lambda expression, function pointers, and even member functions. Here is an example use `std::function` instead of traditional function pointer.

```cpp
int foo(){return 0;}
/* C style */
int (*fp)(int) = foo;
/* std::function */
std::functionfunc = foo;
```

## Create Callback in C++

If you are familiar with Javascript, you may have known the concept of Callback.

```javascript
function Callback() {
    console.log("I am executed!");
}
Callback();
```

In brief, a Callback is a function that passed to another function as parameter and the function is not executed until the callback is called. Before `std::function` is introduced, this is achieved through interface and abstract classes, which can sometimes be a dirty solution and cause a lot of chaos. For example, we can create a call back for a class method without using interface.

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

In the above example, if callback is not used we can still run `foo` by passing a function pointer. However, to execute `bar::run` we will have to inherit `executer` so that it will accept `bar` as parameter since non-static member function can not be referenced.

## Usage of std::bind

The function of `std::bind` is to bind parameters and the function to create a new functor. It is also able to only bind part of the parameter list and even reorder the parameter by using `std::placeholders`.

```cpp
int foo(int a, int b){return a-b;}

class Bar{
public:
    int run(int a){return a;}
};

int main(){
    /* _1 _2 ... _N is from here */
    using namespace std::placeholders;
    /* foo(b, a) */
    auto f1 = std::bind(foo,_2,_1);
    /* foo(a, 3) */
    auto f2 = std::bind(foo,_1, 3);    
    /* foo(1, 3) */
    auto f3 = std::bind(foo, 1, 3);
    /* return bar.run(a) */
    Bar b;
    auto f4 = std::bind(&Bar::run, &b, _1);
    /* print 1 0 -2 6*/
    printf("%d %d %d %d",
           f1(1,2), f2(3,1), f3(-1,-2, 5), f4(6));
}
```

`std::bind` accepts both placeholders and real objects. If it binds a placeholder to function, You have to pass the parameter when calling the generated function. Any other parameters are ignored. It is also possible to bind a member function with the actually class object.

