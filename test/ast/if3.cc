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
            ExprStmt(+(lit:'10', lit:'1'))
            IfStmt(=(id:'a', id:'b'), Block({
               ExprStmt(+(id:'cout', id:'x'))
            }), Block({
               ExprStmt(+(lit:'1', lit:'10'))
            }))
         }))
      })
   })
}
[[err]]--------------------------------------------------
