void incr(int& i) {
    i += 5;
}

int main() {
    incr(1);
}
[[out]]--------------------------------------------------
Program{
    FuncDecl(id:'incr', Type(id:'void'), Params = {"i": Type<&>(id:'int')}, {
        Block({
            ExprStmt(+=(id:'i', Int<5>))
        })
    })
    FuncDecl(id:'main', Type(id:'int'), Params = {}, {
        Block({
            ExprStmt(CallExpr(id:'incr', Args = {Int<1>}))
        })
    })
}
