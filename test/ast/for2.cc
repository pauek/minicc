void print() {
   for (int i = 1; i < 100; i = i + 1) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'print', Type(id:'void'), Params = {}, {
      Block({
         IterStmt<for>(DeclStmt(Type(id:'int'), Vars = {"i" = Int<1>}), <(id:'i', Int<100>), =(id:'i', +(id:'i', Int<1>)), {
            Block({
               ExprStmt(<<(<<(id:'cout', id:'i'), id:'endl'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
