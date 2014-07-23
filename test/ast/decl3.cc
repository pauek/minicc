void f() {
   void a=b,b=c=d,c=d||x,d=2&u;
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("f", Type(void), Params = {}, {
      Block({
         DeclStmt(Type(void), Vars = {"a" = id:'b', "b" = =(id:'c', id:'d'), "c" = ||(id:'d', id:'x'), "d" = &(Int<2>, id:'u')})
      })
   })
}
[[err]]--------------------------------------------------
