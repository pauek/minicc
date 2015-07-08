void f() {
   int j;
   j++;
}
[[err]]--------------------------------------------------
semantic/unknown/incr1.cc[3:3-3:6]: Incrementas la variable 'j' sin haberla inicializado.
