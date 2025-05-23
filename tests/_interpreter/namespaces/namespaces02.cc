int main() {
   foo::bar X;
}
[[err]]--------------------------------------------------
tests/interpreter/namespaces/namespaces02.cc[1:0-1:0]: El tipo 'foo::bar' no existe.
