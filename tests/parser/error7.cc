#include <iostream>
#include <math.h>
using namespace std;

int main()
{
    cout.setf(ios::fixed);
    cout.precision(6);

    int x;
    while (cin >> x) {
        cout << (int)pow(x, 2) << " " << sqrt(double(x)) << endl;
    }
}
[[err]]----------------------------------------------
tests/parser/error7.cc[12:22-12:22]: Esperaba un ';'.
