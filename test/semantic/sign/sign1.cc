void f() {
   bool b = true;
   bool c = -b;
}
[[err]]--------------------------------------------------
semantic/sign/sign1.cc[3:12-3:14]: El cambio de signo para 'bool' no tiene sentido.
