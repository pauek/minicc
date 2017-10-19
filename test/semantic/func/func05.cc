int& f(int& z) {
   return 3;
}
[[err]]--------------------------------------------------
semantic/func/func05.cc[2:4-2:12]: Se devuelve un 'int' cuando debería ser un 'int&'.
