int main() { if (a) break; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<break>())
      })
   })
}
[[err]]------------------------------------------
