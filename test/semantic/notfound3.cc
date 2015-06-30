#include <iostream>
int main() {
   std::string s;
   std::cout << s.len() << std::endl;
}
[[err]]----------------------------------------------------
semantic/notfound3.cc[4:16-4:21]: La clase 'string' no tiene miembro 'len'.
