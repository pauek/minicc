#include <iostream>
#include <map>
using namespace std;

int main() {
   pair<int,int> p = {1, 3}, q = {3, 4}, r = {1, 2};
   cout << (p < q) << (p < r) << (q < r) << endl;
}
[[out]]--------------------------------------------------
100
