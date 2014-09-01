void f() { a + b + c; }
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'f', Type(id:'void'), Params = {}, {
      Block({
         ExprStmt(+(+(id:'a', id:'b'), id:'c'))
      })
   })
}
[[err]]--------------------------------------------------
