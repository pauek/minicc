#include <iostream>
using namespace blah;

double fn() {
   ;    // blah
}
[[out]]--------------------------------------------------
#include <iostream>
using namespace blah;

double fn() {
   ; // blah
}
[[err]]--------------------------------------------------
