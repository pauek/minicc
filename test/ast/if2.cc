int f() {
   if (a != b) {
      cout + x;
   } else {
      1 + 10;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         IfStmt(!=(id:'a', id:'b'), Block({
            ExprStmt(+(id:'cout', id:'x'))
         }), Block({
            ExprStmt(+(Int<1>, Int<10>))
         }))
      })
   })
}
[[err]]--------------------------------------------------
