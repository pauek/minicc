void f() {
   int a;
   a++;
}
[[err]]--------------------------------------------------
tests/semantic/incr/incr3.cc[3:4-3:7]: Incrementas la variable 'a' sin haberla inicializado.
