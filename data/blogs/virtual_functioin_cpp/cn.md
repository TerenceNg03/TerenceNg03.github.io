# C++虛函數的性能問題

## 什么是虚函数？

根据 [cppreference.com](https://en.cppreference.com/w/cpp/language/virtual) 的定义，虚函数是成员函数，其行为可以在派生类中被覆盖。当你尝试将基类和不同的派生类放入容器或作为返回值时，这非常有用，在这种情况下，我们只能使用基类指针引用派生类类型。

大多数编译器使用 VTable 实现虚函数。VTable 是一个表格，存储所有虚函数的地址。因此，虚函数调用将导致两次内存访问。与只需要单个内存访问的普通函数调用相比，这显然不太高效，可能不太友好。

## 性能问题

虚函数有一个非常著名的性能问题。当 VTable 相当大时，这种问题尤其明显。实际上，这会影响编译时间和执行时间。对于执行时间，主要有两个原因。首先，它需要两次内存访问。另一个原因是，它有效地阻止编译器内联函数，这可能导致一些疯狂的优化。让我们亲自测试一下。

首先，设置两个版本的类，一个带有虚函数，一个没有。

```cpp
#ifdef VIR
class Base{
public:
    int i=0;
    virtual void increase(void){i++;};
};

template 
class Derive : public Base{
    virtual void increase(void){i+=inc;};
};

#else
class Base{
public:
    int i=0;
    void increase(void){i++;};
};

template 
class Derive : public Base{
    void increase(void){i+=inc;};
};
#endif
```

为了创建一个大的 VTable，我将利用一些模板元编程。请注意，模板元编程通常会导致非常长的编译时间。
```cpp
template 
class test{
public:
    Derive d;
    test* t;
    test(void){t = new test();}
#ifdef VIR
    void run(){Base*b = &d;b->increase();t->run();}
#else
    void run(){d.increase();t->run();}
#endif
};

template <>
class test<0>{
public:
    Derive<0> d;
    test<0>* t;
    test(){t = NULL;}
    void run(){}
};

int main(){
    test<1024> test_case;
    test_case.run();
}
```

这里我们生成了 1024 个不同的派生类，让我们测试编译和运行需要多长时间。

```plaintext
% time clang++ -DVIR performance.cpp 
real    1187ms
user    1123ms
sys        61ms
% time ./a.out                  
real    1173ms
user    6ms
sys        2ms
% time clang++ performance.cpp 
real    1078ms
user    1019ms
sys        57ms
% time ./a.out            
real    945ms
user    4ms
sys        2ms
```

结果表明，虚函数显着地减慢了编译器和输出程序的速度，分别为约 100 毫秒和近 200 毫秒。在现实中，这可能会引起问题，因为 cpp 通常要求性能高，编译速度慢。

## 是否有替代方案？

简而言之，答案是有，也有没有。确实有一些方法可以绕过虚函数。但我认为没有一种完美的方法可以做到这一点。替代方案通常包括使用函数指针或手动编写 switch 语句或模板。这里我将列出一些可能的解决方案。

## Switch 解决方案

```cpp
class Base{
    enum class type {t1,t2};
    type T;
    void func(){
        switch (T) {
            case type::t1:
                do1();
                break;
            case type::t2:
                do2();
                break;
        }
    }
};
```

这相对简单。使用标记存储类类型并相应地调用不同的函数。但这更像是编写纯 c 代码而不是 OOP 编程语言。这有点繁琐，但你确实获得了一些性能，因为内联函数是可能的。

## 模板解决方案

```cpp
template 
class Base{
    int i;
    void func(){i++;};
};
class Derive{};
template<> void Base<Derive>::func(){i+=2;}
```

模板解决方案包括将基类变为模板类，然后为派生类专门化模板。然而，这不允许您在容器中混合派生类。

## 函数指针解决方案

```cpp
class Base{
public:
    int i;
    Base(){
        i=0;increase = &_increase;
    }
    void (* increase)(Base&b);
    static void _increase(Base&b){b.i=b.i+1;}
};

class Derive: public Base{
public:
    Derive(){
        increase = &_increase;
    };
    static void _increase(Base&b){b.i=b.i+2;}
};
```

在这里，我们需要存储指向原始虚函数的函数指针。并根据不同的类更改此指针。此函数必须是静态的，因此其类型为 `void(*)(Base&b)` 而不是 `void(Derive::*)(Base&b)`，并将对象作为其中一个参数接受。这允许您使用指向基类的指针调用派生类函数，但是此函数只能对基类成员执行操作。

