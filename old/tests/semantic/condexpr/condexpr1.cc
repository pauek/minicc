int main() {
   bool b = ("hola" ? true : false);
}
[[err]]--------------------------------------------------
tests/semantic/condexpr/condexpr1.cc[2:14-2:20]: Debe haber un 'bool' antes del interrogante.
