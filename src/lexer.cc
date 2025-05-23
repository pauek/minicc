#include "lexer.hh"
#include <assert.h>
#include <sstream>
#include "ast.hh"
using namespace std;

void Lexer::_error(string msg) {
    cerr << _pos << ": " << msg << endl;
    exit(1);
}

void Lexer::consume(string word) {
    for (char c : word) {
        consume(c);
    }
}

void Lexer::consume(Token::Type type) {
    auto tok = read_token();
    assert(tok.type == type);
}

string Lexer::substr(const Token& t) {
    return _text.substr(_pos_to_idx(t.pos), t.len);
}

string Lexer::substr(const Pos& ini, const Pos& fin) const {
    const int i = _pos_to_idx(ini);
    const int j = _pos_to_idx(fin);
    assert(j > i and i != -1 and j != -1);
    return _text.substr(i, j - i);
}

CommentSeq *Lexer::skip(Skip skip) {
    CommentSeq *cs = 0;
    int         endls_in_a_row = 0;
    while (!end()) {
        while (curr() == '/') {
            peek(1);
            if (cs == 0) {
                cs = new CommentSeq();
            }
            if (curr(1) == '*') {
                cs->comments.push_back(Comment(Comment::MultiLine));
                read_MultiLine_comment(cs->comments.back());
                endls_in_a_row = 0;
            } else if (curr(1) == '/') {
                cs->comments.push_back(Comment(Comment::SingleLine));
                read_SingleLine_comment(cs->comments.back());
                endls_in_a_row = 0;
            } else {
                break;
            }
        }
        if (curr() == '\r') {
            next();
        }
        if (curr() == '\n') {
            endls_in_a_row++;
            if (cs == 0) {
                cs = new CommentSeq();
            }
            // WTF! Fix this shit...
            if (endls_in_a_row < 3) {
                cs->comments.push_back(Comment(Comment::EndLine));
            }
        } else {
            endls_in_a_row = 0;
        }
        switch (skip) {
            case Skip::SpaceTabNewline:
                if (!(curr() == ' ' || curr() == '\t' || curr() == '\n' || curr() == '\r')) {
                    goto finish;  // break would break from the switch only
                }
                break;
            case Skip::SpaceTab:
                if (!(curr() == ' ' || curr() == '\t')) {
                    goto finish;
                }
                break;
        }
        next();
    }
finish:
    return cs;
}

string Lexer::skip_to(string stop_set) {
    string s;
    while (!end() and (stop_set.find(curr()) == string::npos)) {
        s += curr();
        next();
    }
    return s;
}

void Lexer::save() {
    _stack.push_back(SavedItem(_curr, _pos, _linepos));
}

void Lexer::restore() {
    assert(!_stack.empty());
    SavedItem item = _stack.back();
    _curr = item.curr;
    _pos = item.pos;
    _linepos = item.linepos;
    _stack.pop_back();
}

void Lexer::discard() {
    _stack.pop_back();
}

bool Lexer::next() {
    if (_in == nullptr) {
        return false;
    }
    if (_curr == -1) {
        _pos = Pos(1, 1);
        _linepos.push_back(0);
    } else if (_curr < _text.size() && _text[_curr] == '\n') {
        _pos.lin++;
        _pos.col = 1;
        _linepos.push_back(_curr + 1);
        _seen_endl = true;
    } else {
        _pos.col++;
    }
    _curr++;
    assert(_curr <= _text.size());
    if (_curr == _text.size()) {
        string line;
        if (!getline(*_in, line)) {
            return false;
        }
        _text += line;
        if (!_in->eof()) {
            _text += "\n";
        }
    }
    return true;
}

Token Lexer::read_token() {
#define RESULT1(type)           \
    do {                        \
        Token tok(Token::type); \
        int   ini = _curr;      \
        tok.pos = _pos;         \
        next();                 \
        tok.len = _curr - ini;  \
        return tok;             \
    } while (0)

#define RESULT2(type)           \
    do {                        \
        Token tok(Token::type); \
        int   ini = _curr;      \
        tok.pos = _pos;         \
        next(), next();         \
        tok.len = _curr - ini;  \
        return tok;             \
    } while (0)

#define RESULT_1_2(ch, type1, type2) \
    if (curr(1) == ch)               \
        RESULT2(type2);              \
    else                             \
        RESULT1(type1);

#define RESULT_OP_EQUALS(ch, type1, type2, type3) \
    if (curr(1) == ch)                            \
        RESULT2(type2);                           \
    else if (curr(1) == '=')                      \
        RESULT2(type3);                           \
    else                                          \
        RESULT1(type1);

    switch (curr()) {
        case '(':
            RESULT1(LParen);
        case ')':
            RESULT1(RParen);
        case '[':
            RESULT1(LBracket);
        case ']':
            RESULT1(RBracket);
        case '{':
            RESULT1(LBrace);
        case '}':
            RESULT1(RBrace);
        case ';':
            RESULT1(SemiColon);
        case '#':
            RESULT1(Sharp);
        case ',':
            RESULT1(Comma);
        case '?':
            RESULT1(QMark);
            // TODO: Add '~'
            // case '~': RESULT1(Tilde, Operator, "~");
        case ':':
            RESULT_1_2(':', Colon, ColonColon);
        case '=':
            RESULT_1_2('=', Eq, EqEq);
        case '!':
            RESULT_1_2('=', Excl, ExclEq);
        case '*':
            RESULT_1_2('=', Star, StarEq);
        case '/':
            RESULT_1_2('=', Slash, SlashEq);
        case '%':
            RESULT_1_2('=', Div, DivEq);
        case '^':
            RESULT_1_2('=', Xor, XorEq);
        case '+':
            RESULT_OP_EQUALS('+', Plus, PlusPlus, PlusEq);
        case '|':
            RESULT_OP_EQUALS('|', Bar, BarBar, BarEq);
        case '&':
            RESULT_OP_EQUALS('&', Amp, AmpAmp, AmpEq);
        case '.': {
            if (isdigit(curr(1))) {
                return read_number_literal();
            }
            RESULT1(Dot);
        }
        case '<': {  // < <= << <<= > >= >> >>=
            Token tok;
            tok.pos = _pos;
            int ini = _curr;
            if (curr(1) == '<') {
                if (curr(2) == '=') {  // <<=
                    tok.type = Token::LShiftEq;
                    next(), next(), next();
                } else {  // <<
                    tok.type = Token::LShift;
                    next(), next();
                }
            } else if (curr(1) == '=') {  // <=
                tok.type = Token::LE;
                next(), next();
            } else {
                tok.type = Token::LT;
                next();
            }
            tok.len = _curr - ini;
            return tok;
        }
        case '>': {
            Token tok;
            tok.pos = _pos;
            int ini = _curr;
            if (curr(1) == '>') {
                if (curr(2) == '=') {  // >>=
                    tok.type = Token::RShiftEq;
                    next(), next(), next();
                } else {  // >>
                    tok.type = Token::RShift;
                    next(), next();
                }
            } else if (curr(1) == '=') {  // >=
                tok.type = Token::GE;
                next(), next();
            } else {
                tok.type = Token::GT;
                next();
            }
            tok.len = _curr - ini;
            return tok;
        }
        case '-': {  // - -- -= ->
            Token tok;
            tok.pos = _pos;
            int ini = _curr;
            next();
            switch (curr()) {
                case '=':
                    tok.type = Token::MinusEq;
                    next();
                    break;
                case '-':
                    tok.type = Token::MinusMinus;
                    next();
                    break;
                case '>':
                    tok.type = Token::Arrow;
                    next();
                    break;
                default:
                    tok.type = Token::Minus;
                    break;
            }
            tok.len = _curr - ini;
            return tok;
        }
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            return read_number_literal();
        case 'u':
        case 'U':
        case 'l':
        case 'L':  // char-lit, string-lit, long
            switch (curr(1)) {
                case '\'':
                    return read_string_or_char_literal('\'');
                case '"':
                    return read_string_or_char_literal('"');
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    return read_number_literal();
                default:
                    return read_ident();
            }
        case '"':
            return read_string_or_char_literal('"');
        case '\'':
            return read_string_or_char_literal('\'');
        default: {
            return read_ident();
        }
    }
}

Token Lexer::peek_token() {
    save();
    skip();
    Token tok = read_token();
    restore();
    return tok;
}

int Lexer::_pos_to_idx(Pos p) const {
    if (p.lin < 1 || p.lin >= _linepos.size()) {
        return -1;
    }
    int lini = _linepos[p.lin];
    int lfin = _text.size();
    if (p.lin < _linepos.size() - 1) {
        lfin = _linepos[p.lin + 1];
    }
    const int lsize = lfin - lini;
    if (p.col < 1 || p.col > lsize) {
        return -1;
    }
    return lini + p.col - 1;
}

bool Lexer::peek(int offset) {
    if (_in == 0 || !_in->good()) {
        return false;
    }
    int k = _curr + offset;
    if (k >= _text.size()) {
        string line;
        if (!getline(*_in, line)) {
            return false;
        }
        _text += line;
        if (!_in->eof()) {
            _text += "\n";
        }
    }
    return k < _text.size();
}

bool Lexer::expect(string word) {
    Pos p = _pos;
    int c = _curr;
    for (int i = 0; i < word.size(); i++) {
        if (curr() != word[i]) {
            _curr = c;
            _pos = p;
            return false;
        }
        next();
    }
    return true;
}

bool Lexer::expect(Token::Type type) {
    save();
    skip();
    Token tok = read_token();
    if (tok.type == type) {
        discard();
        return true;
    }
    restore();
    return false;
}

bool Lexer::expectOneOf(const vector<Token::Type>& types) {
    save();
    skip();
    Token tok = read_token();
    auto  it = find(types.begin(), types.end(), tok.type);
    if (it != types.end()) {
        discard();
        return true;
    }
    restore();
    return false;
}

// read_*
void Lexer::read_SingleLine_comment(Comment& c) {
    consume("//");
    while (!end() and curr() != '\n') {
        c.text += curr();
        next();
    }
    return;
}

void Lexer::read_MultiLine_comment(Comment& c) {
    consume("/*");
    while (!end()) {
        if (curr() == '*') {
            peek(1);
            if (curr(1) == '/') {
                consume("*/");
                return;
            }
        }
        c.text += curr();
        next();
    }
    _error("unfinished comment");
    return;
}

inline bool IsUpper(char c) {
    return c >= 'A' and c <= 'Z';
}

inline bool IsLower(char c) {
    return c >= 'a' and c <= 'z';
}

inline bool IsDigit(char c) {
    return c >= '0' and c <= '9';
}

Token Lexer::read_ident() {
    Token tok;
    tok.pos = _pos;
    int  ini = _curr;
    char c = curr();
    if (!IsUpper(c) and !IsLower(c) and c != '_') {
        return Token();
    }
    next();
    c = curr();
    while (IsUpper(c) or IsLower(c) or IsDigit(c) or c == '_') {
        next();
        c = curr();
    }
    // tok.fin = _curr;
    tok.len = _curr - ini;
    tok.type = Token::Ident;
    string s = substr(tok);
    switch (tok.len) {
        case 2: {
            if (s == "or") {
                tok.type = Token::Or;
            } else if (s == "if") {
                tok.type = Token::If;
            }
            break;
        }
        case 3: {
            if (s == "and") {
                tok.type = Token::And;
            } else if (s == "int") {
                tok.type = Token::Int;
            } else if (s == "for") {
                tok.type = Token::For;
            } else if (s == "not") {
                tok.type = Token::Not;
            }
            break;
        }
        case 4: {
            if (s == "else") {
                tok.type = Token::Else;
            } else if (s == "goto") {
                tok.type = Token::Goto;
            } else if (s == "enum") {
                tok.type = Token::Enum;
            } else if (s == "void") {
                tok.type = Token::Void;
            } else if (s == "long") {
                tok.type = Token::Long;
            } else if (s == "bool") {
                tok.type = Token::Bool;
            } else if (s == "char") {
                tok.type = Token::Char;
            } else if (s == "auto") {
                tok.type = Token::Auto;
            } else if (s == "true") {
                tok.type = Token::True;
            }
            break;
        }
        case 5: {
            if (s == "while") {
                tok.type = Token::While;
            } else if (s == "break") {
                tok.type = Token::Break;
            } else if (s == "using") {
                tok.type = Token::Using;
            } else if (s == "short") {
                tok.type = Token::Short;
            } else if (s == "class") {
                tok.type = Token::Class;
            } else if (s == "float") {
                tok.type = Token::Float;
            } else if (s == "const") {
                tok.type = Token::Const;
            } else if (s == "false") {
                tok.type = Token::False;
            }
            break;
        }
        case 6: {
            if (s == "switch") {
                tok.type = Token::Switch;
            } else if (s == "return") {
                tok.type = Token::Return;
            } else if (s == "double") {
                tok.type = Token::Double;
            } else if (s == "string") {
                tok.type = Token::String;
            } else if (s == "struct") {
                tok.type = Token::Struct;
            } else if (s == "signed") {
                tok.type = Token::Signed;
            } else if (s == "static") {
                tok.type = Token::Static;
            } else if (s == "extern") {
                tok.type = Token::Extern;
            } else if (s == "inline") {
                tok.type = Token::Inline;
            }
            break;
        }
        default:
            if (s == "continue") {
                tok.type = Token::Continue;
            } else if (s == "typedef") {
                tok.type = Token::Typedef;
            } else if (s == "mutable") {
                tok.type = Token::Mutable;
            } else if (s == "virtual") {
                tok.type = Token::Virtual;
            } else if (s == "continue") {
                tok.type = Token::Continue;
            } else if (s == "register") {
                tok.type = Token::Register;
            } else if (s == "explicit") {
                tok.type = Token::Explicit;
            } else if (s == "unsigned") {
                tok.type = Token::Unsigned;
            } else if (s == "volatile") {
                tok.type = Token::Volatile;
            } else if (s == "namespace") {
                tok.type = Token::Namespace;
            }
            break;
    }
    return tok;
}

Token Lexer::read_string_or_char_literal(char delim) {
    string str;
    Token  t;
    int    ini = _curr + 1;
    if (curr() == 'L') {
        next();  // TODO: Handle 'L'
    }
    consume(delim);
    t.pos = _pos;
    while (curr() != delim) {
        if (curr() == '\\') {
            next();
            switch (curr()) {
                case 'a':
                case 'b':
                case 'f':
                case 'n':
                case 'r':
                case 't':
                case 'v':
                case '\'':
                case '\"':
                case '\?':
                case '\\':
                    break;
                default:
                    cerr << "warning: unknown Escape sequence '\\" << curr() << "'" << endl;
            }
        } else if (curr() == '\n') {
            _error("string inacabado");
            break;
        }
        next();
    }
    t.len = _curr - ini;
    if (curr() == delim) {
        consume(delim);
    }
    t.type = (delim == '"' ? Token::StringLiteral : Token::CharLiteral);
    return t;
}

Token Lexer::read_number_literal() {
    Token t;
    t.pos = _pos;
    int ini = _curr;
    if (curr() == '.') {
        next();
        return read_float_literal(t, ini);
    }
    while (isdigit(curr())) {
        next();
    }
    if (curr() == '.' || curr() == 'e') {
        next();
        return read_float_literal(t, ini);
    }
    // t.fin = _curr;
    t.len = _curr - ini;
    t.type = Token::IntLiteral;
    return t;
}

Token Lexer::read_float_literal(Token t, int ini) {
    while (isdigit(curr())) {
        next();
    }
    bool isfloat = false;
    if (curr() == 'f') {
        isfloat = true;
        next();
    }
    t.len = _curr - ini;
    t.type = (isfloat ? Token::FloatLiteral : Token::DoubleLiteral);
    return t;
}
