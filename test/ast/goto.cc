int main() { if (a) goto blah; }
[[out]]------------------------------------------
Program{
   FuncDecl("main", Type(id:'int'), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<goto>("blah"))
      })
   })
}
[[err]]------------------------------------------
