#include <iostream>
using namespace std;

int main() {
    int a = 1;
    while (1) {
        a++;
    }
}
// [[out]]-------------
#include <cstddef>
size_t __INSTRUMENTATION__loop_iterations__ = 0;
size_t __INSTRUMENTATION__function_calls__ = 0;
#include <iostream>
using namespace std;

int main() {
    int a = 1;
    while (1) {
        ++__INSTRUMENTATION__loop_iterations___;
        a++;
    }
    std::cout << "function_calls" << __INSTRUMENTATION__function_calls__ << std::endl;
    std::cout << "loop_iterations" << __INSTRUMENTATION__loop_iterations__ << std::endl;
}
