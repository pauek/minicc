int f() {
   if (a == b == c) { cout + x; } else { 10 + 1;
      if (a = b) { cout + x; } else { 1 + 10; } }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(int), Params = {}, {
      Block({
         IfStmt(==(==(id:'a', id:'b'), id:'c'), Block({
            ExprStmt(+(id:'cout', id:'x'))
         }), Block({
            ExprStmt(+(Int<10>, Int<1>))
            IfStmt(=(id:'a', id:'b'), Block({
               ExprStmt(+(id:'cout', id:'x'))
            }), Block({
               ExprStmt(+(Int<1>, Int<10>))
            }))
         }))
      })
   })
}
[[err]]--------------------------------------------------
