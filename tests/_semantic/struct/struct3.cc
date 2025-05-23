struct X {
   int a, b, c, a;
};
[[err]]--------------------------------------------------
tests/semantic/struct/struct3.cc[2:4-2:18]: El campo 'a' está repetido.
