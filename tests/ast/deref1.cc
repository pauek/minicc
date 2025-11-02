double    fn   (char c1 ,char   c2  )    {
    *c1;
}
[[out]]------------------------------------
Program{
    FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
        Block({
            ExprStmt(DerefExpr(id:'c1'))
        })
    })
}
