#include <iostream>
using namespace std;
int main(int a  ,/* bla */  int    b   )  {}
[[out]]----------------------------------------
#include <iostream>
using namespace std;

int main(int a, /* bla */ int b) {}
[[err]]----------------------------------------
