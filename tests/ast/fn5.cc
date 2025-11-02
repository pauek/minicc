struct Blah {
    int x, y;
};

const Bleh& f();
[[out]]----------------------------------------
Program{
    StructDecl('Blah', {
        DeclStmt(Type(id:'int'), Vars = {"x", "y"})
    })
    FuncDecl(id:'f', Type<&>(id:'Bleh', {const}), Params = {})
}
