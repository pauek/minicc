int main() { if (a) goto blah; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(int), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<goto>("blah"))
      })
   })
}
[[err]]------------------------------------------
