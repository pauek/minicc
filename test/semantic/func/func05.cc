int& f(int& z) {
   return 3;
}
[[err]]--------------------------------------------------
semantic/func/func05.cc[2:3-2:11]: Se devuelve un 'int' cuando debería ser un 'int&'.
