int main() { if (a) break; }
[[out]]------------------------------------------
Program{
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         IfStmt(id:'a', JumpStmt<break>())
      })
   })
}
[[err]]------------------------------------------
