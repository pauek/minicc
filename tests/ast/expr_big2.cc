double    fn   (char c1 ,char   c2  )    {
    a || false = b * c + d && x ^ y;
}
[[out]]------------------------------------
Program{
    FuncDecl(id:'fn', Type(id:'double'), Params = {"c1": Type(id:'char'), "c2": Type(id:'char')}, {
        Block({
            ExprStmt(=(||(id:'a', Bool<false>), &&(+(*(id:'b', id:'c'), id:'d'), ^(id:'x', id:'y'))))
        })
    })
}
