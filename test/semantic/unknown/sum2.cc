void f() {
   int j;
   j = 1 + j;
}
[[err]]--------------------------------------------------
semantic/unknown/sum2.cc[3:11-3:12]: Utilizas la variable 'j' sin haberla inicializado.
