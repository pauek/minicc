struct X {
   int a, b, c;
   int a;
};
[[err]]--------------------------------------------------
tests/semantic/struct/struct4.cc[3:4-3:9]: El campo 'a' está repetido.
