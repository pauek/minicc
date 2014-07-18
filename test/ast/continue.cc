int main() { if (a) continue; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<continue>())
      })
   })
}
[[err]]------------------------------------------
