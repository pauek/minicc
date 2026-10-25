#include <iostream>
int main() {
   std::string s;
   std::cout << s.len() << std::endl;
}
[[err]]----------------------------------------------------
tests/semantic/notfound/notfound3.cc[4:17-4:22]: La clase 'string' no tiene método 'len'.
