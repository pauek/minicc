int main() {
   const int a = 7;
   a = 3;
}
[[err]]--------------------------------------------------
semantic/const/const1.cc[3:3-3:8]: La variable 'a' no se puede modificar (es 'const').
