void print() {
   for (int i = 1, j = i; i < 100 && j < 100; i = i + 1, j = j + 1) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl("print", Type(void), Params = {}, {
      Block({
         IterStmt<for>(DeclStmt(Type(int), Vars = {"i" = lit:'1', "j" = id:'i'}), &&(<(id:'i', lit:'100'), <(id:'j', lit:'100')), ,(=(id:'i', +(id:'i', lit:'1')), =(id:'j', +(id:'j', lit:'1'))), {
            Block({
               ExprStmt(<<(<<(id:'cout', id:'i'), id:'endl'))
            })
         })
      })
   })
}
[[err]]--------------------------------------------------
