void f() {
   bool b = true;
   bool c = -b;
}
[[err]]--------------------------------------------------
tests/semantic/sign/sign1.cc[3:13-3:15]: El cambio de signo para 'bool' no tiene sentido.
