#include <iostream>
using namespace std;
int main(int a  , int   /* bla */ b   )  {}
[[out]]----------------------------------------
#include <iostream>
using namespace std;

int main(int a, int /* bla */ b) {}
[[err]]----------------------------------------
