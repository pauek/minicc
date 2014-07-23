int main() { if (a) break; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(id:'int'), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<break>())
      })
   })
}
[[err]]------------------------------------------
