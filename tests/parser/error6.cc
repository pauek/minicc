#include <cmath>
#include <iostream>
using namespace std;

int main()
{
    cout.setf(ios::fixed);
    cout.precision(6);

    int x;
    while (cin >> x) {
        int quadrat = x * x;
        double arrel = sqrt(x);
        cout << quadrat << " " << arrel << endl;
    }
}
