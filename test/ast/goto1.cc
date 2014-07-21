int main() { if (a) goto xxx; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<goto>("xxx"))
      })
   })
}
[[err]]------------------------------------------
