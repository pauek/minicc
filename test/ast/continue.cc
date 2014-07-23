int main() { if (a) continue; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(id:'int'), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<continue>())
      })
   })
}
[[err]]------------------------------------------
