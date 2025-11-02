#include <iostream>
using namespace std;

void f() {}

int main() {
    int a = 1;
    for (int i = 0; i < 100; i++) {
        if (i % 2 == 0) {
            f();
        }
    }
}
/* [[out]]-------------
function_calls 50
loop_iterations 100
[[end]] */