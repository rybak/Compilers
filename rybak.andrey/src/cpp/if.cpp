#include <cstdio>

int less(int a, int b) {
    if (a > b) {
        return 1;
    } else {
        if (b > a) {
            return -1;
        } else {
            return 0;
        }
    }
}
int main() {
    printf("%d\n", less(10, 4));
    printf("%d\n", less(3, 5));
}
