#include <iostream>
using namespace std;

int main() {
    int a[2] = {1, 1};
    int b[2][2];
    int c[2][3][4];
    int d[2][2][5][10];
}
// [[out]]
Program{
    Include(<iostream>)
    Using(std)
    FuncDecl(id:'main', Type(id:'int'), Params = {}, {
        Block({
            DeclStmt(Type(id:'int'), Vars = {"a"(Size = Int<2>) = {Int<1>, Int<1>}})
            DeclStmt(Type(id:'int'), Vars = {"b"(Sizes = {Int<2>, Int<2>})})
            DeclStmt(Type(id:'int'), Vars = {"c"(Sizes = {Int<2>, Int<3>, Int<4>})})
            DeclStmt(Type(id:'int'), Vars = {"d"(Sizes = {Int<2>, Int<2>, Int<5>, Int<10>})})
        })
    })
}
