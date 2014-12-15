#include <iostream>
#include <list>
#include <sstream>
using namespace std;

bool more(int a, int b) {
   return a > b;
}

int main() {
   list<int> L;
   int n;
   while (cin >> n) { 
      L.push_back(n); 
   }
   L.sort(more);
   list<int>::iterator it;
   for (it = L.begin(); it != L.end(); it++) {
      cout << *it << ' ';
   }
   cout << endl;
}
[[in]]---------------------------------------------------
9 4 7 2 8 0 1 8 5 3 4
[[out]]--------------------------------------------------
9 8 8 7 5 4 4 3 2 1 0 
