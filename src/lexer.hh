#ifndef INPUT_H
#define INPUT_H
#include <cassert>
#include <iostream>
#include <vector>
#include "pos.hh"
#include "token.hh"

struct Comment;
struct CommentSeq;

class Lexer {
    std::istream    *_in;
    std::string      _text;
    int              _curr;
    Pos              _pos;
    std::vector<int> _linepos;  // positions of line starts (ignoring position 0 since no line 0)

    struct SavedItem {
        int              curr;
        Pos              pos;
        std::vector<int> linepos;

        SavedItem(int c, Pos p, const std::vector<int>& l) : curr(c), pos(p), linepos(l) {}
    };

    std::vector<SavedItem> _stack;  // save/restore stack
    bool                   _seen_endl;

    void _reset() {
        _pos.lin = 1;
        _pos.col = -1;
        _curr = -1;
    }

    int _pos_to_idx(Pos p) const;

    void _error(std::string msg);

   public:
    Lexer() : _in(0), _linepos(1) { _reset(); }

    Lexer(std::istream *i) : _in(i), _linepos(1) { _reset(); }

    bool next();
    bool peek(int offset);

    Pos pos() const { return _pos; }

    char curr(int i = 0) const { return _text[_curr + i]; }

    bool end() const { return _curr >= _text.size(); }

    bool        curr_one_of(std::string set) const;
    std::string substr(const Token& t);
    std::string substr(const Pos& begin, const Pos& end) const;

    std::string substr(const Span& r) const { return substr(r.begin, r.end); }

    void save();
    void restore();
    void discard();

    void mark() { _seen_endl = false; }

    bool expect(Token::Type type);
    bool expect(std::string word);

    void consume(char c) {
        assert(curr() == c);
        next();
    }

    void consume(std::string word);
    void consume(Token::Type type);

    enum class Skip {
        SpaceTabNewline,
        SpaceTab,
    };

    CommentSeq *skip(Skip skip = Skip::SpaceTabNewline);
    std::string skip_to(std::string stop_set);

    Token read_token();
    Token peek_token();
    Token read_ident();
    Token read_number_literal();
    Token read_float_literal(Token t, int ini);
    Token read_string_or_char_literal(char delim);
    void  read_SingleLine_comment(Comment& c);
    void  read_MultiLine_comment(Comment& c);
};

#endif
