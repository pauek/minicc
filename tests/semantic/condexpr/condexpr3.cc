int main() {
   int a = (true ? 1 : 2);
}
[[err]]--------------------------------------------------
tests/semantic/condexpr/condexpr3.cc[2:12-2:26]: La condición siempre es 'true'.
