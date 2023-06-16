# C++虛函數的性能問題

## 什麼是虛函數？

根據 [cppreference.com](https://en.cppreference.com/w/cpp/language/virtual) 的定義，虛函數是成員函數，其行為可以在派生類中被覆蓋。當你嘗試將基類和不同的派生類放入容器或作為返回值時，這非常有用，在這種情況下，我們只能使用基類指針引用派生類類型。

大多數編譯器使用 VTable 實現虛函數。VTable 是一個表格，存儲所有虛函數的地址。因此，虛函數調用將導致兩次內存訪問。與只需要單個內存訪問的普通函數調用相比，這顯然不太高效，可能不太友好。

## 性能問題

虛函數有一個非常著名的性能問題。當 VTable 相當大時，這種問題尤其明顯。實際上，這會影響編譯時間和執行時間。對於執行時間，主要有兩個原因。首先，它需要兩次內存訪問。另一個原因是，它有效地阻止編譯器內聯函數，這可能導致一些瘋狂的優化。讓我們親自測試一下。

首先，設置兩個版本的類，一個帶有虛函數，一個沒有。

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

為了創建一個大的 VTable，我將利用一些模板元編程。請注意，模板元編程通常會導致非常長的編譯時間。
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

這裡我們生成了 1024 個不同的派生類，讓我們測試編譯和運行需要多長時間。

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

結果表明，虛函數顯著地減慢了編譯器和輸出程序的速度，分別為約 100 毫秒和近 200 毫秒。在現實中，這可能會引起問題，因為 cpp 通常要求性能高，編譯速度慢。

## 是否有替代方案？

簡而言之，答案是有，也有沒有。確實有一些方法可以繞過虛函數。但我認為沒有一種完美的方法可以做到這一點。替代方案通常包括使用函數指針或手動編寫 switch 語句或模板。這裡我將列出一些可能的解決方案。

## Switch 解決方案

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

這相對簡單。使用標記存儲類類型並相應地調用不同的函數。但這更像是編寫純 c 代碼而不是 OOP 編程語言。這有點繁瑣，但你確實獲得了一些性能，因為內聯函數是可能的。

## 模板解決方案

```cpp
template 
class Base{
    int i;
    void func(){i++;};
};
class Derive{};
template<> void Base<Derive>::func(){i+=2;}
```

模板解決方案包括將基類變為模板類，然後為派生類專門化模板。然而，這不允許您在容器中混合派生類。

## 函數指針解決方案

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

在這裡，我們需要存儲指向原始虛函數的函數指針。並根據不同的類更改此指針。此函數必須是靜態的，因此其類型為 `void(*)(Base&b)` 而不是 `void(Derive::*)(Base&b)`，並將對象作為其中一個參數接受。這允許您使用指向基類的指針調用派生類函數，但是此函數只能對基類成員執行操作。

