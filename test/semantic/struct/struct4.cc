struct X {
   int a, b, c;
   int a;
};
[[err]]--------------------------------------------------
semantic/struct/struct4.cc[3:3-3:8]: El campo 'a' está repetido.
