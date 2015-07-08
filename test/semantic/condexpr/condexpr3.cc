int main() {
   int a = (true ? 1 : 2);
}
[[err]]--------------------------------------------------
semantic/condexpr/condexpr3.cc[2:11-2:25]: La condición siempre es 'true'.
