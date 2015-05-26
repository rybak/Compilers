#include <cstdio>

int plus(int a, int b)
{
    int c = a + b;
    return c;
}
int minus(int a, int b)
{
    int c = a - b;
    return c;
}
int main()
{
    int (*foo)(int,int);
    int a = 10;
    int b = 5;
    if (a > b) {
        foo = plus;
    } else {
        foo = minus;
    }
    std::printf("%d", foo(a,b));
    return 0;
}
