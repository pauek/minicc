#include <iostream>
using namespace std;
int main(int a  , int    b /* bla */  )  {}
[[out]]----------------------------------------
#include <iostream>
using namespace std;

int main(int a, int b /* bla */ ) {}
[[err]]----------------------------------------
