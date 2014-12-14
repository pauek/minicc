#include <iostream>
#include <vector>
using namespace std;

typedef vector<vector<int>> vvi;

int main() {
   vvi a(2);
   a[0].push_back(-2);
   a[1].push_back(103);
   a[1].push_back(107);
   cout << a[0].size() << ": " << a[0][0] << endl;
   cout << a[1].size() << ": " << a[1][0] << ' ' << a[1][1] << endl;
}
[[out]]--------------------------------------------------
1: -2
2: 103 107
