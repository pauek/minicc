int main() {
   bool b = ("hola" ? true : false);
}
[[err]]--------------------------------------------------
semantic/condexpr/condexpr1.cc[2:13-2:19]: Debe haber un 'bool' antes del interrogante.
