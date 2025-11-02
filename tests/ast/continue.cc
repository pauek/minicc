int main() { if (a) continue; }
[[out]]------------------------------------------
Program{
    FuncDecl(id:'main', Type(id:'int'), Params = {}, {
        Block({
            IfStmt(id:'a', JumpStmt<continue>())
        })
    })
}
