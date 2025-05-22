#include <iostream>
using namespace std;

int main() {
    int x, pos = 1;
    while (cin >> x && x % 2 == 1) {
        pos++;
    }
    cout << pos << endl;
}
