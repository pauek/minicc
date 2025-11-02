#include <iostream>
using namespace blah;

double    fn   (char c1 ,char   c2  )    {
    a= /* b + */ a+ 1 ;
}
[[out]]--------------------------------------------------
#include <iostream>
using namespace blah;

double fn(char c1, char c2) {
    a = /* b + */ a + 1;
}
