int main() { if (a) goto xxx; }
[[out]]------------------------------------------
Program{
    FuncDecl(id:'main', Type(id:'int'), Params = {}, {
        Block({
            IfStmt(id:'a', JumpStmt<goto>("xxx"))
        })
    })
}
