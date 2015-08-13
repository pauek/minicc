void f(double& a, double& b) {
   a += b;
}

void g() {
   double x;
   f(x, 5);
}
[[err]]----------------------------------------------------
semantic/func/params3.cc[7:8-7:9]: En el segundo parámetro se requiere una variable.
