int main() {
   bool b;
   b = (b ? true : 7);
}
[[err]]--------------------------------------------------
tests/semantic/condexpr/condexpr2.cc[3:8-3:22]: Los tipos de las dos expresiones alternativas deben coincidir (son 'bool' y 'int').
