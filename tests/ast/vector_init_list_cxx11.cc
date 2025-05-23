int main() {
    vector<double> v { 5, 7, 3.4, 3.3, 4, 9, 2 };
    bla_bla(v);
}
[[out]]----------------------------------------------------
Program{
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'vector'<Type(id:'double')>), Vars = {"v" = {Int<5>, Int<7>, Double<3.4>, Double<3.3>, Int<4>, Int<9>, Int<2>}})
         ExprStmt(CallExpr(id:'bla_bla', Args = {id:'v'}))
      })
   })
}