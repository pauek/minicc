void f() {
   int a;
   a++;
}
[[err]]--------------------------------------------------
semantic/incr/incr3.cc[3:3-3:6]: Incrementas la variable 'a' sin haberla inicializado.
