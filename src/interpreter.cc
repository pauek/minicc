#include "interpreter.hh"
#include <assert.h>
#include <iostream>
#include "ast.hh"
#include "translator.hh"
#include "types.hh"
#include "value.hh"
using namespace std;

void Interpreter::invoke_func_prepare_arg(FuncDecl *fn, Value arg, int i) {
    if (arg.is<Reference>()) {
        if (!fn->params[i]->typespec->reference) {
            arg = Reference::deref(arg);
        }
        setenv(fn->params[i]->name, arg);
    } else {
        if (fn->params[i]->typespec->reference) {
            _error(_T("En el parámetro %d se requiere una variable.", i + 1));
        }
        setenv(fn->params[i]->name, arg);
    }
}

void Interpreter::invoke_func_prepare(FuncDecl *fn, const vector<Value>& args) {
    if (fn->params.size() != args.size()) {
        _error(_T("Error en el número de argumentos al llamar a '%s'", fn->FuncName().c_str()));
    }
    for (int i = 0; i < args.size(); i++) {
        invoke_func_prepare_arg(fn, args[i], i);
    }
}

void Interpreter::program_prepare(Program *X) {
    prepare_global_environment();
    for (AstNode *n : X->nodes) {
        eval(n);
    }
}

void Interpreter::find_main() {
    if (!getenv("main", _curr)) {
        _error(_T("The '%s' function does not exist.", "main"));
    }
    if (!_curr.is<Callable>()) {
        _error(_T("'main' is not a function."));
    }
}

struct _Add {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a + b;
    }
};

struct _Sub {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a - b;
    }
};

struct _Mul {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a * b;
    }
};

struct _Div {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a / b;
    }
};

struct _And {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a & b;
    }
};

struct _Or {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a | b;
    }
};

struct _Xor {
    template <typename TestClass>
    static TestClass eval(const TestClass& a, const TestClass& b) {
        return a ^ b;
    }
};

struct _AAdd {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a += b;
    }
};

struct _ASub {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a -= b;
    }
};

struct _AMul {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a *= b;
    }
};

struct _ADiv {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a /= b;
    }
};

struct _AAnd {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a &= b;
    }
};

struct _AOr {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a |= b;
    }
};

struct _AXor {
    template <typename TestClass>
    static void eval(TestClass& a, const TestClass& b) {
        a ^= b;
    }
};

struct _Lt {
    template <typename TestClass>
    static bool eval(const TestClass& a, const TestClass& b) {
        return a < b;
    }
};

struct _Le {
    template <typename TestClass>
    static bool eval(const TestClass& a, const TestClass& b) {
        return a <= b;
    }
};

struct _Gt {
    template <typename TestClass>
    static bool eval(const TestClass& a, const TestClass& b) {
        return a > b;
    }
};

struct _Ge {
    template <typename TestClass>
    static bool eval(const TestClass& a, const TestClass& b) {
        return a >= b;
    }
};

template <class Op>
bool Interpreter::eval_op_assignment(Value left, Value _right) {
    Value right = left.type()->convert(_right);
    if (left.is<Int>() and right.is<Int>()) {
        Op::eval(left.as<Int>(), right.as<Int>());
        left.touch();
        return true;
    }
    if (left.is<Float>() and right.is<Float>()) {
        Op::eval(left.as<Float>(), right.as<Float>());
        left.touch();
        return true;
    }
    if (left.is<Double>() and right.is<Double>()) {
        Op::eval(left.as<Double>(), right.as<Double>());
        left.touch();
        return true;
    }
    return false;
}

template <class Op>
bool Interpreter::eval_bitop_assignment(Value left, Value _right) {
    Value right = left.type()->convert(_right);
    if (left.is<Int>() and right.is<Int>()) {
        Op::eval(left.as<Int>(), right.as<Int>());
        left.touch();
        return true;
    }
    return false;
}

template <class Op>
bool Interpreter::eval_sum_prod(Value left, Value _right) {
    Value right = left.type()->convert(_right);
    if (left.is<Int>()) {
        _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
        return true;
    }
    if (left.is<Float>()) {
        _curr = Value(Op::eval(left.as<Float>(), right.as<Float>()));
        return true;
    }
    if (left.is<Double>()) {
        _curr = Value(Op::eval(left.as<Double>(), right.as<Double>()));
        return true;
    }
    return false;
}

template <class Op>
bool Interpreter::eval_bitop(Value left, Value right) {
    if (left.is<Int>() and right.is<Int>()) {
        _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
        return true;
    }
    return false;
}

template <class Op>
bool Interpreter::eval_comparison(Value left, Value right) {
    if (left.is<Int>() and right.is<Int>()) {
        _curr = Value(Op::eval(left.as<Int>(), right.as<Int>()));
        return true;
    }
    if (left.is<Float>() and right.is<Float>()) {
        _curr = Value(Op::eval(left.as<Float>(), right.as<Float>()));
        return true;
    }
    if (left.is<Double>() and right.is<Double>()) {
        _curr = Value(Op::eval(left.as<Double>(), right.as<Double>()));
        return true;
    }
    if (left.is<String>() and right.is<String>()) {
        _curr = Value(Op::eval(left.as<String>(), right.as<String>()));
        return true;
    }
    return false;
}

void Interpreter::eval_binary_expr_assignment(Value left, Value right) {
    if (!left.is<Reference>()) {
        _error(_T("Intentas asignar sobre algo que no es una variable"));
    }
    left = Reference::deref(left);
    right = left.type()->convert(right);
    if (right == Value::null) {
        // TODO: Find operator as method or function
        _error(
            _T("La asignación no se puede hacer porque los "
               "tipos no son compatibles (%s) vs (%s)",
               left.type_name().c_str(),
               right.type_name().c_str())
        );
    }
    if (!left.assign(right)) {
        _error(
            _T("La asignación no se puede hacer porque los "
               "tipos no son compatibles (%s) vs (%s)",
               left.type_name().c_str(),
               right.type_name().c_str())
        );
    }
    _curr = left;
}

void Interpreter::eval_binary_expr_op_assignment(char op, Value left, Value right) {
    if (!left.is<Reference>()) {
        _error(_T("Para usar '%s=' se debe poner una variable a la izquierda", op));
    }
    left = Reference::deref(left);
    bool ok = false;
    switch (op) {
        case '+': {
            // FIXME: use 'string::operator+='
            if (left.is<String>() and right.is<String>()) {
                left.as<String>() += right.as<String>();
                ok = true;
            } else if (left.is<String>() and right.is<Char>()) {
                left.as<String>() += right.as<Char>();
                ok = true;
            } else {
                ok = eval_op_assignment<_AAdd>(left, right);
            }
            break;
        }
        case '-':
            ok = eval_op_assignment<_ASub>(left, right);
            break;
        case '*':
            ok = eval_op_assignment<_AMul>(left, right);
            break;
        case '/':
            ok = eval_op_assignment<_ADiv>(left, right);
            break;
        case '&':
            ok = eval_bitop_assignment<_AAnd>(left, right);
            break;
        case '|':
            ok = eval_bitop_assignment<_AOr>(left, right);
            break;
        case '^':
            ok = eval_bitop_assignment<_AXor>(left, right);
            break;
    }
    if (!ok) {
        string _op = "?=";
        _op[0] = op;
        _error(_T("Los operandos de '%s' no son compatibles", _op.c_str()));
    }
}

bool Interpreter::call_operator(string op, const vector<Value>& args) {
    if (!bind_field(_curr, op)) {
        return false;
    }
    if (_curr.is<Overloaded>()) {
        _curr = _curr.as<Overloaded>().resolve(args);
        assert(_curr.is<Callable>());
    }
    Binding&        opfun = _curr.as<Callable>();
    const Function *func_type = opfun.func.type()->as<Function>();
    check_arguments(func_type, args);
    _curr = opfun.call(args);
    return true;
}

void Interpreter::invoke_user_func(FuncDecl *decl, const vector<Value>& args) {
    pushenv(decl->FuncName());
    invoke_func_prepare(decl, args);
    eval(decl->block);
    popenv();
}

void Interpreter::get_func(CallExpr *X) {
    eval(X->func);
    _curr = Reference::deref(_curr);
    if (!_curr.is<Callable>() and !_curr.is<Overloaded>()) {
        _error(_T("Calling something other than a function."));
    }
}

void Interpreter::check_arguments(const Function *func_type, const vector<Value>& args) {
    for (int i = 0; i < args.size(); i++) {
        const Type *param_type = func_type->param(i);
        if (param_type == Any) {
            continue;
        }
        string t1 = param_type->TypeStr();
        Value  arg_i = args[i];
        if (!func_type->param(i)->is<Reference>()) {
            arg_i = Reference::deref(arg_i);
        } else if (!arg_i.type()->is<Reference>()) {
            _error(_T("En el parámetro %d se requiere una variable.", i + 1));
        }
        string t2 = arg_i.type()->TypeStr();
        if (t1 != t2) {
            _error(
                _T("El argumento %d no es compatible con el tipo del parámetro "
                   "(%s vs %s)",
                   i + 1,
                   t1.c_str(),
                   t2.c_str())
            );
        }
    }
}

void Interpreter::eval_arguments(const std::vector<Expr *>& exprs, std::vector<Value>& args) {
    for (int i = 0; i < exprs.size(); i++) {
        eval(exprs[i]);
        args.push_back(_curr);
    }
}

void Interpreter::check_result(Binding& fn, const Function *func_type) {
    if (_ret == Value::null && !func_type->is_void()) {
        string      name = fn.func.as<Function>().ptr->name;
        const Type *return_type = func_type->return_type();
        _error(_T(
            "La función '%s' debería devolver un '%s'", name.c_str(), return_type->TypeStr().c_str()
        ));
    }
}

bool Interpreter::type_conversion(CallExpr *X, const vector<Value>& args) {
    if (is_a<Identifier>(X->func)) {
        Identifier *id = cast<Identifier>(X->func);
        TypeSpec    spec(id);
        const Type *type = get_type(&spec);
        if (type != 0) {
            if (args.size() != 1) {
                _error(_T("La conversión de tipo recibe un solo argumento"));
            }
            _curr = type->convert(args[0]);
            if (_curr == Value::null) {
                _curr = args[0];
                call_operator(id->TypeStr());
            }
            return true;
        }
    }
    return false;
}

void Interpreter::call(Value func, const vector<Value>& args) {
    // TODO: Find operator() (method or function)
    if (func.is<Overloaded>()) {
        func = func.as<Overloaded>().resolve(args);
        assert(func.is<Callable>());
    }
    Binding&        fn = func.as<Callable>();
    const Function *func_type = fn.func.type()->as<Function>();
    check_arguments(func_type, args);
    _ret = fn.call(args);  // <-- Invoke!
    check_result(fn, func_type);
    _curr = _ret;
}

bool Interpreter::bind_field(Value obj, string method_name) {
    vector<Value> candidates;
    int           count = obj.type()->get_field(obj, method_name, candidates);
    if (count == 1) {
        Value& v = candidates[0];
        if (v.is<Function>()) {
            _curr = Callable::self->mkvalue(obj, v);
        } else {
            _curr = v;
        }
        return true;
    } else if (count > 1) {
        _curr = Overloaded::self->mkvalue(obj, candidates);
        return true;
    }
    return false;
}

// Eval
void Interpreter::eval(AstNode *ast) {
    assert(ast != nullptr);
    switch (ast->type()) {
        case AstNodeType::Program: {
            Program *X = cast<Program>(ast);
            program_prepare(X);
            find_main();
            _curr.as<Callable>().call(vector<Value>());
            break;
        }
        case AstNodeType::Macro:
            break;
        case AstNodeType::Using: {
            Using *X = cast<Using>(ast);
            if (!using_namespace(X->namespc)) {
                _error(_T("No se ha encontrado el \"namespace\" '%s'.", X->namespc.c_str()));
            }
            break;
        }
        case AstNodeType::Include: {
            Include *X = cast<Include>(ast);
            include_header_file(X->filename);
            break;
        }
        case AstNodeType::FuncDecl: {
            FuncDecl *X = cast<FuncDecl>(ast);
            string    funcname = X->FuncName();
            auto     *return_type = get_type(X->return_typespec);  // return_type == 0 means 'void'
            Function *functype = new Function(return_type);
            for (auto p : X->params) {
                const Type *param_type = get_type(p->typespec);
                assert(param_type != 0);
                functype->add_params(param_type);
            }
            Value func = functype->mkvalue(new UserFunc(funcname, X, this));
            Value callable = Callable::self->mkvalue(Value::null, func);  // bind with 'null'
            setenv(funcname, callable, Hidden);
            break;
        }
        case AstNodeType::StructDecl: {
            StructDecl *X = cast<StructDecl>(ast);
            Struct     *type = new Struct(X->name);
            for (int i = 0; i < X->decls.size(); i++) {
                DeclStmt&   decl = *X->decls[i];
                const Type *field_type = get_type(decl.typespec);
                assert(type != 0);
                for (DeclStmt::Item& item : decl.items) {
                    if (is_a<ArrayDecl>(item.decl)) {
                        Expr    *size_expr = cast<ArrayDecl>(item.decl)->sizes[0];
                        Literal *size_lit = cast<Literal>(size_expr);
                        assert(size_lit != 0);
                        assert(size_lit->kind == Literal::Int);
                        const int sz = size_lit->val.as_int;
                        // TODO: don't create new Array type every time?
                        type->add_field(item.decl->name, new Array(field_type, sz));
                    } else {
                        type->add_field(item.decl->name, field_type);
                    }
                }
            }
            register_type(X->name, type);
            break;
        }
        case AstNodeType::Identifier: {
            Identifier *X = cast<Identifier>(ast);
            Value       v;
            // Try a namespace
            Identifier *namespc_or_class = X->GetPotentialNamespaceOrClass();
            if (namespc_or_class != 0) {
                Environment *namespc = get_namespace(namespc_or_class->name);
                if (namespc != 0) {
                    if (namespc->get(X->name, v)) {
                        goto found;
                    }
                    _error(
                        _T("No se ha encontrado '%s' en el namespace '%s'.",
                           X->name.c_str(),
                           namespc_or_class->name.c_str())
                    );
                    return;
                }
            }
            // Try a static variable in a class
            if (namespc_or_class != 0) {
                Identifier fid;
                fid.name = namespc_or_class->name;
                TypeSpec    spec(&fid);
                const Type *type = get_type(&spec);
                if (type != 0 and !type->get_static(X->name, v)) {
                    _error(
                        _T("No se ha encontrado '%s' en la clase '%s'.",
                           X->name.c_str(),
                           namespc_or_class->name.c_str())
                    );
                }
                goto found;
            }
            // Try the environment
            if (getenv(X->name, v)) {
                goto found;
            }
            _error(_T("No se ha encontrado '%s'.", X->name.c_str()));
        found:
            _curr = (v.is<Reference>() ? v : Reference::mkref(v));
            break;
        }
        case AstNodeType::Literal: {
            Literal *X = cast<Literal>(ast);
            switch (X->kind) {
                case Literal::String:
                    _curr = Value(*X->val.as_string.s);
                    break;
                case Literal::Int:
                    _curr = Value(X->val.as_int);
                    break;
                case Literal::Double:
                    _curr = Value(X->val.as_double);
                    break;
                case Literal::Bool:
                    _curr = Value(X->val.as_bool);
                    break;
                case Literal::Char:
                    _curr = Value(X->val.as_char);
                    break;
                default:
                    _error(_T("Interpreter::visit_literal: UNIMPLEMENTED"));
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(ast);
            eval(X->left);
            Value left = _curr;
            if (X->kind != Expr::Eq) {
                left = Reference::deref(left);
            }
            // Handle booleans first, since they might not need to evaluate the
            // second part
            //
            if (X->op == "&&" or X->op == "and" or X->op == "||" or X->op == "or") {
                if (left.is<Bool>(
                    ) /* and right.is<Bool>() // FIXME: Check in SemanticAnalyzer!! */) {
                    // do not evaluate right hand side if already enough with left
                    if (X->op == "&&" or X->op == "and") {
                        if (!left.as<Bool>()) {
                            _curr = Value(false);
                            return;
                        }
                    } else {
                        if (left.as<Bool>()) {
                            _curr = Value(true);
                            return;
                        }
                    }
                    eval(X->right);
                    Value right = _curr;
                    right = Reference::deref(right);
                    _curr = Value(
                        X->op == "&&" or X->op == "and" ? left.as<Bool>() and right.as<Bool>()
                                                        : left.as<Bool>() or right.as<Bool>()
                    );
                    return;
                }
                _error(_T("Los operandos de '%s' no son de tipo 'bool'", X->op.c_str()));
            }
            eval(X->right);
            Value right = _curr;
            right = Reference::deref(right);
            if (X->op == ",") {
                return;  // already evaluated
            }
            if (X->op == "=") {
                eval_binary_expr_assignment(left, right);
                return;
            }
            if (X->op == "+=" || X->op == "-=" || X->op == "*=" || X->op == "/=" || X->op == "&=" ||
                X->op == "|=" || X->op == "^=") {
                eval_binary_expr_op_assignment(X->op[0], left, right);
                return;
            } else if (X->op == "&" || X->op == "|" || X->op == "^") {
                bool ret = false;
                switch (X->op[0]) {
                    case '&':
                        ret = eval_bitop<_And>(left, right);
                        break;
                    case '|':
                        ret = eval_bitop<_Or>(left, right);
                        break;
                    case '^':
                        ret = eval_bitop<_Xor>(left, right);
                        break;
                }
                if (ret) {
                    return;
                }
                _error(_T("Los operandos de '%s' son incompatibles", X->op.c_str()));
            } else if (X->op == "+" || X->op == "*" || X->op == "-" || X->op == "/") {
                bool ret = false;
                if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
                    switch (X->op[0]) {
                        case '+': {
                            if (left.is<Char>() and right.is<Int>()) {
                                _curr = Value(char(left.as<Char>() + right.as<Int>()));
                                return;
                            } else {
                                ret = eval_sum_prod<_Add>(left, right);
                                break;
                            }
                        }
                        case '*':
                            ret = eval_sum_prod<_Mul>(left, right);
                            break;
                        case '-':
                            ret = eval_sum_prod<_Sub>(left, right);
                            break;
                        case '/':
                            ret = eval_sum_prod<_Div>(left, right);
                            break;
                    }
                } else {
                    _curr = left;
                    if (!call_operator(X->op, vector<Value>(1, right))) {
                        _error(
                            _T("El tipo '%s' no tiene 'operator%s'",
                               _curr.type()->TypeStr().c_str(),
                               X->op.c_str())
                        );
                    }
                    ret = true;
                }
                if (ret) {
                    return;
                }
                _error(_T("Los operandos de '%s' son incompatibles", X->op.c_str()));
            } else if (X->op == "%") {
                if (left.is<Int>() and right.is<Int>()) {
                    _curr = Value(left.as<Int>() % right.as<Int>());
                    return;
                }
                _error(_T("Los operandos de '%s' son incompatibles", "%"));
            } else if (X->op == "%=") {
                if (!left.is<Reference>()) {
                    _error(_T(
                        "Para usar '%s' se debe poner una variable a la izquierda", X->op.c_str()
                    ));
                }
                left = Reference::deref(left);
                if (left.is<Int>() and right.is<Int>()) {
                    left.as<Int>() %= right.as<Int>();
                    return;
                }
                _error(_T("Los operandos de '%s' son incompatibles", "%="));
            } else if (X->op == "==" || X->op == "!=") {
                if (left.same_type_as(right)) {
                    _curr = Value(X->op == "==" ? left.equals(right) : !left.equals(right));
                    return;
                }
                _error(_T("Los operandos de '%s' no son del mismo tipo", X->op.c_str()));
            } else if (X->op == "<" || X->op == ">" || X->op == "<=" || X->op == ">=") {
                bool ret = false;
                if (left.type()->is(Type::Basic) and right.type()->is(Type::Basic)) {
                    if (X->op[0] == '<') {
                        ret =
                            (X->op.size() == 1 ? eval_comparison<_Lt>(left, right)
                                               : eval_comparison<_Le>(left, right));
                    } else {
                        ret =
                            (X->op.size() == 1 ? eval_comparison<_Gt>(left, right)
                                               : eval_comparison<_Ge>(left, right));
                    }
                } else {
                    _curr = left;
                    if (!call_operator(X->op, vector<Value>(1, right))) {
                        _error(
                            _T("El tipo '%s' no tiene 'operator%s'",
                               _curr.type()->TypeStr().c_str(),
                               X->op.c_str())
                        );
                    }
                    ret = true;
                }
                if (ret) {
                    return;
                }
                // TODO: Find operator as method or function
                _error(_T("Los operandos de '%s' no son compatibles", X->op.c_str()));
            }
            _curr = left;
            if (call_operator(X->op, vector<Value>(1, right))) {
                return;
            }
            _error(_T("Interpreter::visit_binaryexpr: UNIMPLEMENTED (%s)", X->op.c_str()));
            break;
        }
        case AstNodeType::Block: {
            Block *X = cast<Block>(ast);
            for (Stmt *stmt : X->stmts) {
                eval(stmt);
            }
            break;
        }
        case AstNodeType::VarDecl: {
            VarDecl    *X = cast<VarDecl>(ast);
            string      type_name = X->typespec->TypeStr();
            const Type *type = get_type(X->typespec);
            if (type == 0) {
                _error(_T("El tipo '%s' no existe.", type_name.c_str()));
            }
            try {
                Value init = (_curr.is_null() ? type->create() : type->convert(_curr));
                if (X->typespec->HasQualifier(TypeSpec::Const)) {
                    init.set_const(true);
                }
                setenv(X->name, init);
            } catch (TypeError& e) {
                _error(e.msg);
            }
            break;
        }
        case AstNodeType::ArrayDecl: {
            ArrayDecl  *X = cast<ArrayDecl>(ast);
            Value       init = _curr;
            vector<int> sizes;
            for (int i = 0; i < X->sizes.size(); i++) {
                eval(X->sizes[i]);
                _curr = Reference::deref(_curr);
                if (!_curr.is<Int>()) {
                    _error(_T("El tamaño de una tabla debe ser un entero"));
                }
                if (_curr.as<Int>() <= 0) {
                    _error(_T("El tamaño de una tabla debe ser un entero positivo"));
                }
                const int sz = _curr.as<Int>();
                sizes.push_back(sz);
            }
            const Type *celltype = get_type(X->typespec);
            if (celltype == 0) {
                _error(_T("El tipo '%s' no existe", X->typespec->TypeStr().c_str()));
            }
            // TODO: don't create new Array type every time?
            const Type *arraytype = Array::mkarray(celltype, sizes);
            setenv(X->name, (init.is_null() ? arraytype->create() : arraytype->convert(init)));
            break;
        }
        case AstNodeType::ObjDecl: {
            ObjDecl    *X = cast<ObjDecl>(ast);
            const Type *type = get_type(X->typespec);
            if (type != 0) {
                vector<Value> args;
                eval_arguments(X->args, args);
                string constructor_name = type->name();
                Value  new_obj = type->create();
                if (!bind_field(new_obj, constructor_name)) {
                    _error(_T("El tipo '%s' no tiene constructor", type->TypeStr().c_str()));
                }
                if (_curr.is<Overloaded>()) {
                    _curr = _curr.as<Overloaded>().resolve(args);
                    assert(_curr.is<Callable>());
                }
                Binding&        constructor = _curr.as<Callable>();
                const Function *func_type = constructor.func.type()->as<Function>();
                check_arguments(func_type, args);
                constructor.call(args);  // <-- Invoke!
                setenv(X->name, new_obj);
                return;
            }
            _error(_T("The type '%s' is not implemented in MiniCC", X->typespec->TypeStr().c_str())
            );
            break;
        }
        case AstNodeType::DeclStmt: {
            DeclStmt *X = cast<DeclStmt>(ast);
            for (DeclStmt::Item& item : X->items) {
                if (item.init) {
                    eval(item.init);
                } else {
                    _curr = Value::null;
                }
                eval(item.decl);
            }
            break;
        }
        case AstNodeType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(ast);
            if (X->expr) {
                eval(X->expr);
                if (X->is_return) {
                    _ret = _curr;
                }
            }
            break;
        }
        case AstNodeType::IfStmt: {
            IfStmt *X = cast<IfStmt>(ast);
            eval(X->cond);
            _curr = Reference::deref(_curr);
            if (!_curr.is<Bool>()) {
                if (!call_operator("bool")) {
                    _error(_T("An if's condition needs to be a bool value"));
                }
            }
            if (_curr.as<Bool>()) {
                eval(X->then);
            } else {
                if (X->els != 0) {
                    eval(X->els);
                }
            }
            break;
        }
        case AstNodeType::ForStmt: {
            ForStmt *X = cast<ForStmt>(ast);
            pushenv("");
            if (X->init) {
                eval(X->init);
            }
            while (true) {
                eval(X->cond);
                _curr = Reference::deref(_curr);
                if (!_curr.is<Bool>()) {
                    if (!call_operator("bool")) {
                        _error(_T("La condición de un for debe ser un valor de tipo bool."));
                    }
                }
                if (!_curr.as<Bool>()) {
                    break;
                }
                eval(X->substmt);
                if (X->post) {
                    eval(X->post);
                }
            }
            popenv();
            break;
        }
        case AstNodeType::WhileStmt: {
            WhileStmt *X = cast<WhileStmt>(ast);
            pushenv("");
            while (true) {
                eval(X->cond);
                _curr = Reference::deref(_curr);
                if (!_curr.is<Bool>()) {
                    if (!call_operator("bool")) {
                        _error(_T("La condición de un while debe ser un valor de tipo bool."));
                    }
                }
                if (!_curr.as<Bool>()) {
                    break;
                }
                eval(X->substmt);
            }
            popenv();
            break;
        }
        case AstNodeType::CallExpr: {
            CallExpr     *X = cast<CallExpr>(ast);
            vector<Value> args;
            eval_arguments(X->args, args);
            if (type_conversion(X, args)) {
                return;
            }
            get_func(X);
            call(_curr, args);
            break;
        }
        case AstNodeType::IndexExpr: {
            IndexExpr *X = cast<IndexExpr>(ast);
            eval(X->base);
            Value base = Reference::deref(_curr);
            eval(X->index);
            Value index = Reference::deref(_curr);
            if (base.is<Array>()) {
                if (!index.is<Int>()) {
                    _error(_T("El índice en un acceso a tabla debe ser un entero"));
                }
                vector<Value>& vals = base.as<Array>();
                const int      i = index.as<Int>();
                if (i < 0 || i >= vals.size()) {
                    // TODO: Producir error de ejecución
                    _error(_T("La casilla %d no existe", i));
                }
                _curr = Reference::mkref(vals[i]);
                return;
            }
            _curr = base;
            if (!call_operator("[]", vector<Value>(1, index))) {
                _error(_T("Las expresiones de índice deben usarse sobre tablas o vectores"));
            }
            break;
        }
        case AstNodeType::FieldExpr: {
            FieldExpr *X = cast<FieldExpr>(ast);
            eval(X->base);
            _curr = Reference::deref(_curr);
            if (X->pointer) {
                if (!call_operator("*")) {
                    _error(_T("El tipo '%s' no tiene 'operator*'", _curr.type()->TypeStr().c_str())
                    );
                }
                _curr = Reference::deref(_curr);
            }
            Value obj = _curr;
            if (obj.is<Struct>()) {
                // TODO: Move this to 'get_field' in 'Struct' class???
                SimpleTable<Value>& fields = obj.as<Struct>();
                Value               v;
                if (!fields.get(X->field, v)) {
                    _error(_T("No existe el campo '%s'", X->field.c_str()));
                }
                _curr = Reference::mkref(v);
                return;
            }
            if (!bind_field(obj, X->field)) {
                _error(_T("Este objeto no tiene un campo '%s'", X->field.c_str()));
            }
            break;
        }
        case AstNodeType::CondExpr: {
            CondExpr *X = cast<CondExpr>(ast);
            eval(X->cond);
            _curr = Reference::deref(_curr);
            if (!_curr.is<Bool>()) {
                _error(
                    _T("Una expresión condicional debe tener valor "
                       "de tipo 'bool' antes del interrogante")
                );
            }
            if (_curr.as<Bool>()) {
                eval(X->then);
            } else if (X->els != 0) {
                eval(X->els);
            } else {
                assert(false);
            }
            break;
        }
        case AstNodeType::ExprList: {
            ExprList      *X = cast<ExprList>(ast);
            Value          v = VectorValue::make();
            vector<Value>& vals = v.as<VectorValue>();
            for (Expr *e : X->exprs) {
                eval(e);
                vals.push_back(_curr);
            }
            _curr = v;
            break;
        }
        case AstNodeType::SignExpr: {
            SignExpr *X = cast<SignExpr>(ast);
            eval(X->expr);
            if (X->kind == SignExpr::Positive) {
                return;
            }
            _curr = Reference::deref(_curr);
            if (_curr.is<Int>()) {
                _curr.as<Int>() = -_curr.as<Int>();
            } else if (_curr.is<Float>()) {
                _curr.as<Float>() = -_curr.as<Float>();
            } else if (_curr.is<Double>()) {
                _curr.as<Double>() = -_curr.as<Double>();
            } else {
                _error(
                    _T("El cambio de signo para '%s' no tiene sentido", _curr.type_name().c_str())
                );
            }
            break;
        }
        case AstNodeType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(ast);
            eval(X->expr);
            if (!_curr.is<Reference>()) {
                _error(_T("Hay que incrementar una variable, no un valor"));
            }
            Value after = Reference::deref(_curr);
            Value before = after.clone();
            if (after.is<Int>()) {
                if (X->kind == IncrExpr::Positive) {
                    after.as<Int>()++;
                    after.touch();
                } else {
                    after.as<Int>()--;
                    after.touch();
                }
            } else {
                _curr = after;
                string op = (X->kind == IncrExpr::Positive ? "++" : "--");
                if (!call_operator(op)) {
                    _error(
                        _T("El tipo '%s' no tiene 'operator%s'",
                           _curr.type()->TypeStr().c_str(),
                           op.c_str())
                    );
                }
            }
            _curr = (X->preincr ? before : after);
            break;
        }
        case AstNodeType::NegExpr: {
            NegExpr *X = cast<NegExpr>(ast);
            eval(X->expr);
            if (!_curr.is<Bool>()) {
                _error(_T("Para negar una expresión ésta debe ser de tipo 'bool'"));
            }
            _curr.as<Bool>() = !_curr.as<Bool>();
            break;
        }
        case AstNodeType::TypedefDecl: {
            TypedefDecl *X = cast<TypedefDecl>(ast);
            string       name = X->decl->name;
            const Type  *type = get_type(X->decl->typespec);
            assert(type != 0);
            switch (X->decl->type()) {
                case AstNodeType::VarDecl: {
                    const VarDecl *var = cast<VarDecl>(X->decl);
                    register_type(var->name, type);
                    break;
                }
                case AstNodeType::ArrayDecl: {
                    const ArrayDecl *array = cast<ArrayDecl>(X->decl);
                    eval(array->sizes[0]);
                    if (!_curr.is<Int>()) {
                        _error(_T("El tamaño de un array debería ser un entero"));
                    }
                    const int size = _curr.as<Int>();
                    register_type(array->name, new Array(type, size));
                    break;
                }
                default:
                    assert(false);
            }
            break;
        }
        case AstNodeType::DerefExpr: {
            DerefExpr *X = cast<DerefExpr>(ast);
            eval(X->expr);
            _curr = Reference::deref(_curr);
            if (!call_operator("*")) {
                _error(_T("El tipo '%s' no tiene 'operator*'", _curr.type()->TypeStr().c_str()));
            }
            break;
        }
        default:
            assert(false);
    }
}

void eval(AstNode *ast, std::istream& in, std::ostream& out) {
    Interpreter(&in, &out).eval(ast);
}