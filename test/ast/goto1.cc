int main() { if (a) goto xxx; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(id:'int'), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<goto>("xxx"))
      })
   })
}
[[err]]------------------------------------------
