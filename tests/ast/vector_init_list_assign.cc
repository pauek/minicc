int main() {
    vector<double> v;
    v = {1.0, 4.0, 6.0, 9.0, 10.0, 12.0};
}
[[out]]---------------------------------------
Program{
   FuncDecl(id:'main', Type(id:'int'), Params = {}, {
      Block({
         DeclStmt(Type(id:'vector'<Type(id:'double')>), Vars = {"v"})
         ExprStmt(=(id:'v', {Double<1>, Double<4>, Double<6>, Double<9>, Double<10>, Double<12>}))
      })
   })
}