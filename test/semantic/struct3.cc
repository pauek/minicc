struct X {
   int a, b, c, a;
};
[[err]]--------------------------------------------------
semantic/struct3.cc[2:3-2:17]: El campo 'a' está repetido.
