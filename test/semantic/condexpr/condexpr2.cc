int main() {
   bool b;
   b = (b ? true : 7);
}
[[err]]--------------------------------------------------
semantic/condexpr/condexpr2.cc[3:7-3:21]: Los tipos de las dos expresiones alternativas deben coincidir (son 'bool' y 'int').
