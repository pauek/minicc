int main() {
   for (int i = 0, i < 1, i++) {
      cout << i << endl;
   }
}
[[err]]-----------------------------------------------
tests/parser/for1.cc[6:1-6:1]: Esperaba '}' pero he llegado al final del texto.
tests/parser/for1.cc[4:5-4:5]: Esperaba ')' aquí.
tests/parser/for1.cc[2:13-2:13]: Esperaba ';' después de la expresión.
tests/parser/for1.cc[6:1-6:1]: Esperaba ';' después de la expresión.