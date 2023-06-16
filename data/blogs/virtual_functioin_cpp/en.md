# Virtual Function - Good or Bad

## What is Virtual Function?

According to [cppreference.com](https://en.cppreference.com/w/cpp/language/virtual), virtual functions are member functions whose behavior can be overridden in derived classes. This is especially useful when you are trying to put base class and different derived class into a container or as a return value, in which case all we can do is use a base class pointer reference a derived class type.

Most compilers implement virtual function with VTable. VTable is a table stores the address of all virtual functions. Thus, a call of virtual function will result in two memory accesses. Compared to normal function call which only requires a single memory access, this is obviously not very efficient and potentially not very cache friendly.

## Performance Issue

Virtual functions has a really well-known performance issue. Such an issue is especially noticeable when the VTable is considerably large. Actually, both compilation time and execution time. For execution time there are mainly two reasons. Firstly, it requires two memory accesses. Another reason is that it effectively prevent compiler to inline function which potentially leads to some crazy optimizations. Let us test it for ourselves.

First, set up two versions of class, one with virtual function and one without.

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

In order to create a large VTable, I will exploit some template meta-programming. Note that template meta-programming usually leads to a very long compile time.

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

Here we generate 1024 different derived classes and let us test how long does it take to compile and run.

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

The result illustrates that virtual function is slowing down both compiler and the output program noticeably, with about 100ms and nearly 200ms respectively. In reality, this is likely causing an issue as cpp is usually performance demanding and slow to compile.

## Is there Any Alternatives?

In short, the answer is yes and no. There is indeed some ways to get around virtual functions. But I do not think there is a perfect way to do this. Alternatives generally include using function pointers or a manual written switch statements or a template. Here I will list some of the possible solutions.

## Switch Solution

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

This is relatively simple. Use a tag to store class type and call different functions accordingly. However, this is more like you are writing plain c code rather than an OOP programming language. This is kind of tedious but you do gain some performance as inline function is possible.

## Template Solution

```cpp
template 
class Base{
    int i;
    void func(){i++;};
};
class Derive{};
template<> void Base::func(){i+=2;}
```

Template solution includes make base class a template class and later specialization template for derived class. However, this does not allow you to mix derived classes in a container.

## Function Pointer Solution

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

Here, we need to store a funtion pointer to the orginal virtual function. And change this pointer according to different class. This function must be static so its type is `void(*)(Base&b)` instead of `void(Derive::*)(Base&b)` and accept an object as one of the parameters. This allows you to use pointer to base class to call derived class functions, yet this funtion can only perform operate on base class members.
