void print() {
   for (int i = 1, j = i; i < 100 && j < 100; i = i + 1, j = j + 1) {
      cout << i << endl;
   }
}
[[out]]--------------------------------------------------
Program{
   FuncDecl(id:'print', Type(id:'void'), Params = {}, {
      Block({
         IterStmt<for>(DeclStmt(Type(id:'int'), Vars = {"i" = Int<1>, "j" = id:'i'}), &&(<(id:'i', Int<100>), <(id:'j', Int<100>)), ,(=(id:'i', +(id:'i', Int<1>)), =(id:'j', +(id:'j', Int<1>))), {
            Block({
               ExprStmt(<<(<<(id:'cout', id:'i'), id:'endl'))
            })
         })
      })
   })
}
