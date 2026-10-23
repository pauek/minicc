int f() {
   if (true) { a + 1; }
   else bla + 2;
}
[[out]]--------------------------------------------------
int f() {
   if (true) {
      a + 1;
   }
   else bla + 2;
}
