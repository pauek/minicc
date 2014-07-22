void print() {
   for (int i = 1; i < 100; i = i + 1) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("print", Type(void), Params = {}, {
      Block({
         IterStmt<for>(DeclStmt(Type(int), Vars = {"i" = lit:'1'}), <(id:'i', lit:'100'), =(id:'i', +(id:'i', lit:'1')), {
            Block({
               ExprStmt(<<(<<(id:'cout', id:'i'), id:'endl'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
