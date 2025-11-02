void print() {
    for (; a == b ;) {
        cout << i << endl;
    }
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'print', Type(id:'void'), Params = {}, {
        Block({
            ForStmt(_, ==(id:'a', id:'b'), _, {
                Block({
                    ExprStmt(<<(<<(id:'cout', id:'i'), id:'endl'))
                })
            })
        })
    })
}
