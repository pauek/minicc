#include <stdint.h>
#include <iomanip>
#include <sstream>
#include <vector>
using namespace std;
#include "stepper.hh"
#include "translator.hh"

bool Stepper::step() {
    _err = 0;
    I.clear_touched();
    try {
        Todo t = Next;
        while (!finished() and t == Next) {
            t = _stack.top()->step(this);
        }
        return true;
    } catch (EvalError *e) {
        _err = e;
        return false;
    }
}

string Stepper::output() {
    std::string s = _out.str();
    _out.str("");
    return s;
}

Todo Stepper::PopState::step(Stepper *S) {
    S->pop();
    delete this;
    return Next;
}

void Stepper::generic_visit(AstNode *X) {
    I.eval(X);
    status(describe(X));
    push(new PopState(X->span));
}

void Stepper::Step(AstNode *ast) {
    switch (ast->type()) {
        case AstNodeType::Program: {
            Program *X = cast<Program>(ast);
            I.program_prepare(X);
            I.find_main();
            status(_T("The program begins."));
            I.pushenv("main");
            Func     *fn = I._curr.as<Callable>().func.as<Function>().ptr;
            FuncDecl *main = dynamic_cast<UserFunc *>(fn)->decl;
            I.invoke_func_prepare(main, vector<Value>());
            I.actenv();
            push(new ProgramVisitState(main));
            break;
        }
        case AstNodeType::Block: {
            Block *X = cast<Block>(ast);
            if (!X->stmts.empty()) {
                push(new BlockVisitState(X));
                Step(X->stmts[0]);
            }
            break;
        }
        case AstNodeType::BinaryExpr: {
            BinaryExpr *X = cast<BinaryExpr>(ast);
            generic_visit(X);
            break;
        }
        case AstNodeType::IncrExpr: {
            IncrExpr *X = cast<IncrExpr>(ast);
            generic_visit(X);
            break;
        }
        case AstNodeType::ExprStmt: {
            ExprStmt *X = cast<ExprStmt>(ast);
            if (is_assignment(X->expr)) {
                visit_assignment(cast<BinaryExpr>(X->expr));
            } else if (is_write_expr(X->expr)) {
                BinaryExpr          *e = cast<BinaryExpr>(X->expr);
                WriteExprVisitState *ws = new WriteExprVisitState(e);
                collect_rights(e, ws->exprs);
                push(ws);
                ws->step(this);
            } else if (is_a<CallExpr>(X->expr)) {
                Step(X->expr);
            } else {
                I.eval(X->expr);
                if (X->is_return) {
                    ostringstream oss;
                    oss << I._curr;
                    status(_T("%s is returned.", oss.str().c_str()));
                } else {
                    status(describe(X->expr));
                }
                push(new PopState(X->span));
            }
            break;
        }
        case AstNodeType::DeclStmt: {
            DeclStmt *X = cast<DeclStmt>(ast);
            generic_visit(X);
            break;
        }
        case AstNodeType::IfStmt: {
            IfStmt *X = cast<IfStmt>(ast);
            I.eval(X->cond);
            Value cond = I._curr;
            if (!cond.is<Bool>()) {
                _error(_T("The condition in a '%s' has to be a value of type 'bool'.", "if"));
            }
            Stmt *next;
            if (cond.as<Bool>()) {
                status(_T("The condition is 'true', we take the first branch."));
                next = X->then;
            } else {
                if (X->els != 0) {
                    status(_T("The condition is 'false', we take the second branch."));
                    next = X->els;
                } else {
                    status(_T("The condition is 'false', we continue."));
                    next = 0;
                }
            }
            push(new IfVisitState(X->cond->span, next));
            break;
        }
        case AstNodeType::ForStmt: {
            ForStmt *X = cast<ForStmt>(ast);
            push(new ForVisitState(X));
            Step(X->init);
            break;
        }
        case AstNodeType::WhileStmt: {
            WhileStmt       *X = cast<WhileStmt>(ast);
            WhileVisitState *s = new WhileVisitState(X);
            s->step(this);
            push(s);
            break;
        }
        case AstNodeType::CallExpr: {
            CallExpr     *X = cast<CallExpr>(ast);
            vector<Value> args;
            I.eval_arguments(X->args, args);
            if (I.type_conversion(X, args)) {
                push(new PopState(X->span));
                return;
            }
            I.get_func(X);
            if (I._curr.is<Overloaded>()) {
                I._curr = I._curr.as<Overloaded>().resolve(args);
                assert(I._curr.is<Callable>());
            }
            Func           *fptr = I._curr.as<Callable>().func.as<Function>().ptr;
            const UserFunc *userfunc = dynamic_cast<const UserFunc *>(fptr);
            if (userfunc == 0) {
                I.call(I._curr, args);
                push(new PopState(X->span));
                return;
            }
            FuncDecl *fn = userfunc->decl;
            assert(fn != 0);
            CallExprVisitState *s = new CallExprVisitState(X, fn);
            I.pushenv(fn->FuncName());
            s->step(this);
            push(s);
            break;
        }
        case AstNodeType::Literal: {
            Literal *X = cast<Literal>(ast);
            eval(X);
            break;
        }
        case AstNodeType::FieldExpr: {
            FieldExpr *X = cast<FieldExpr>(ast);
            eval(X);
            break;
        }
        case AstNodeType::Identifier: {
            Identifier *X = cast<Identifier>(ast);
            eval(X);
            break;
        }
        case AstNodeType::IndexExpr: {
            IndexExpr *X = cast<IndexExpr>(ast);
            eval(X);
            break;
        }
        default:
            assert(false);
    }
}

Span Stepper::ProgramVisitState::span() const {
    if (at == ProgramVisitState::Begin) {
        return X->id->span;
    } else if (at == ProgramVisitState::Finished) {
        Span span(X->block->span.end, X->block->span.end);
        span.begin.col--;
        return span;
    }
    return Span();
}

Todo Stepper::ProgramVisitState::step(Stepper *S) {
    switch (at) {
        case Begin: {
            if (X->block->stmts.empty()) {
                at = End;
                return Next;
            }
            S->Step(X->block);
            at = End;
            return Stop;
        }
        case End: {
            at = Finished;
            S->status(_T("The program ends."));
            return Stop;
        }
        case Finished: {
            S->I.popenv();
            S->pop();
            delete this;
            return Next;
        }
        default:
            assert(false);
    }
}

Todo Stepper::BlockVisitState::step(Stepper *S) {
    const int last = X->stmts.size() - 1;
    ++curr;
    if (curr > last) {
        S->pop();
        delete this;
        return Next;
    }
    S->Step(X->stmts[curr]);
    return Stop;
}

Todo Stepper::IfVisitState::step(Stepper *S) {
    S->pop();
    Todo todo = Stop;
    if (next == 0) {
        todo = Next;
    } else {
        S->Step(next);
    }
    delete this;
    return todo;
}

Todo Stepper::ForVisitState::step(Stepper *S) {
    switch (at) {
        case Stepper::ForVisitState::Leave: {
            S->pop();
            delete this;
            return Next;
        }
        case Stepper::ForVisitState::Cond: {
            S->Step(X->cond);
            Value cond = S->I._curr;
            if (!cond.is<Bool>()) {
                S->_error(_T("The condition in a '%s' must be a value of type 'bool'.", "for"));
            }
            if (!cond.as<Bool>()) {
                S->status(_T("The condition is 'false', we exit the %s.", "for"));
                at = Stepper::ForVisitState::Leave;
            } else {
                S->status(_T("The condition is 'true', we enter the %s.", "for"));
                at = Stepper::ForVisitState::Block;
            }
            return Stop;
        }
        case Stepper::ForVisitState::Block: {
            S->Step(X->substmt);
            at = Stepper::ForVisitState::Post;
            return Stop;
        }
        case Stepper::ForVisitState::Post: {
            S->Step(X->post);
            at = Stepper::ForVisitState::Cond;
            return Stop;
        }
        default:
            assert(false);
    }
    return Stop;
}

Todo Stepper::WhileVisitState::step(Stepper *S) {
    Todo todo = Stop;
    switch (at) {
        case Stepper::WhileVisitState::Leave: {
            S->pop();
            delete this;
            todo = Next;
            break;
        }
        case Stepper::WhileVisitState::Cond: {
            S->Step(X->cond);
            Value cond = S->I._curr;
            if (!cond.is<Bool>()) {
                S->_error(_T("The condition in a '%s' must be a value of type 'bool'.", "while"));
            }
            if (!cond.as<Bool>()) {
                S->status(_T("The condition is 'false', we exit the %s.", "while"));
                at = Stepper::WhileVisitState::Leave;
            } else {
                S->status(_T("The condition is 'true', we enter the %s.", "while"));
                at = Stepper::WhileVisitState::Block;
            }
            break;
        }
        case Stepper::WhileVisitState::Block:
            S->Step(X->substmt);
            at = Stepper::WhileVisitState::Cond;
            break;
    }
    return todo;
}

Span Stepper::WriteExprVisitState::span() const {
    return curr->span;
}

Todo Stepper::WriteExprVisitState::step_waiting(Stepper *S) {
    S->status(_T("Some output is written."));
    Value curr = Reference::deref(S->I._curr);
    S->_out << curr;
    exprs.pop_front();
    waiting = false;
    return Stop;
}

Todo Stepper::WriteExprVisitState::step(Stepper *S) {
    if (!exprs.empty()) {
        if (waiting) {
            return step_waiting(S);
        } else {
            curr = exprs.front();
            const int old_sz = S->_stack.size();
            S->Step(curr);
            if (S->_stack.size() > old_sz) {
                waiting = true;
                return Stop;
            } else {
                return step_waiting(S);
            }
        }
    } else {
        S->pop();
        delete this;
        return Next;
    }
}

void Stepper::visit_assignment(BinaryExpr *e) {
    assert(e != 0);
    I.eval(e->right);
    Value         right = Reference::deref(I._curr);
    ostringstream oss;
    oss << right;
    string Escaped = Literal::Escape(oss.str(), '"');
    status(_T("La expresión ha dado %s.", Escaped.c_str()));
    push(new AssignVisitState(e, right));
}

Span Stepper::AssignVisitState::span() const {
    if (left.is_null()) {
        return X->right->span;
    } else {
        return Span(X->left->span.begin, X->right->span.begin);
    }
}

Todo Stepper::AssignVisitState::step(Stepper *S) {
    if (!left.is_null()) {
        S->pop();
        delete this;
        return Next;
    }
    S->I.eval(X->left);
    left = S->I._curr;
    if (X->op == "=") {
        S->I.eval_binary_expr_assignment(left, right);
    } else if (X->op.size() == 2 and X->op[1] == '=') {
        S->I.eval_binary_expr_op_assignment(X->op[0], left, right);
    }
    S->status(_T("We assign the value."));
    return Stop;
}

const int Stepper::CallExprVisitState::Block = -1;
const int Stepper::CallExprVisitState::Return = -2;

Span Stepper::CallExprVisitState::span() const {
    if (curr == CallExprVisitState::Return) {
        return X->span;
    } else if (curr == CallExprVisitState::Block) {
        return fn->id->span;
    } else {
        return X->args[curr - 1]->span;
    }
}

Todo Stepper::CallExprVisitState::step(Stepper *S) {
    const int size = X->args.size();
    if (curr == CallExprVisitState::Return) {
        S->I.popenv();
        S->pop();
        delete this;
        return Next;
    } else if (curr == CallExprVisitState::Block) {
        S->Step(fn->block);
        curr = CallExprVisitState::Return;
        return Stop;
    } else if (curr < size) {
        if (curr < 7) {
            S->status(_T("We evaluate the %s parameter.", _T(numeral[curr + 1]).c_str()));
        } else {
            S->status(_T("We evaluate parameter number %d.", curr));
        }
        S->I.eval(X->args[curr]);
        Value v = S->I._curr;
        S->I.invoke_func_prepare_arg(fn, v, curr);
        ++curr;
        return Stop;
    } else {
        S->status(_T("We jump to function '%s'.", fn->FuncName().c_str()));
        S->I.actenv();
        curr = Block;
        return Stop;
    }
}

string json_encode(string s) {
    ostringstream json;
    for (int i = 0; i < s.size(); i++) {
        if (uint8_t(s[i]) < 0x7F) {
            json << s[i];
        } else if ((uint8_t(s[i]) & 0x000000E0) == 0x000000C0) {
            // Gran cutrada...
            json << "\\u" << hex << setfill('0') << setw(4)
                 << ((uint8_t(s[i] & 0x1F) << 6) + uint8_t(s[i + 1] & 0x3F));
            i++;
        } else {
            assert(false);
        }
    }
    return json.str();
}

string Stepper::state2json() const {
    ostringstream json;
    json << "{";
    json << "\"env\":" << I.env2json() << ",";
    json << "\"status\":" << "\"" << json_encode(_status) << "\",";
    Span s = span();
    json << "\"span\":" << "{";
    // -1 for codemirror!
    json << "\"ini\":{" << "\"line\":" << s.begin.lin - 1 << ",\"ch\":" << s.begin.col << "},";
    json << "\"fin\":{" << "\"line\":" << s.end.lin - 1 << ",\"ch\":" << s.end.col << "}";
    json << "}}";
    return json.str();
}
