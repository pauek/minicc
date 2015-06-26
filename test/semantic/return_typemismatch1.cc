int f(int a) {
   return "hola";
}
[[err]]----------------------------------------------------
semantic/return_typemismatch1.cc[2:3-2:16]: Se devuelve un 'string' cuando debería ser un 'int'.
