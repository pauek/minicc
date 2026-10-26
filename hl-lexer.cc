#include <iostream>
#include <string>

#include "tokens.hh"
using namespace std;

#include "error.hh"
#include "hl-lexer.hh"

struct HLexDumper {
    ostream&      out;
    const Tokens& tokens;
    int           level;

    string prefix(int i) const {
        auto [lin, col] = tokens.token_line_col(i);
        string pref = std::format("{}:{}:{}:", tokens.filename_, lin, col);
        // Assume (naïve) that lin is at most 5 digits and col as well (1 + 5 + 1 + 5 + 1)
        pref.resize(tokens.filename_.size() + 13, ' ');
        return pref;
    }

    string indent() const { return string(level * 4, ' '); }

    HLexDumper(ostream& out, const Tokens& tokens) : out(out), tokens(tokens), level(0) {}

    void dump_tokenseq(const TokenSeq& tseq);
    void dump_groupseq(const GroupSeq& gseq);
    void dump_structured(const Structured& strd);
    void dump(TokenGroup group);
};

void HLexDumper::dump_groupseq(const GroupSeq& gseq) {
    out << prefix(gseq.begin) << indent() << "group:" << endl;
    // out << tokens.filename_ << ":" << tokens.token_line_col(tseq.begin);
    level++;
    for (auto group : gseq.elems) {
        dump(group);
    }
    level--;
}

void HLexDumper::dump_structured(const Structured& strd) {
    out << prefix(strd.begin) << indent() << token_name(strd.delim);
    if (strd.sep != NONE) {
        out << token_name(strd.sep);
    }
    out << token_name(closing(strd.delim));
    if (not strd.groups.empty()) {
        out << ":" << endl;
        level++;
        for (auto group : strd.groups) {
            dump_groupseq(group.get());
        }
        level--;
    } else {
        out << endl;
    }
}

void HLexDumper::dump_tokenseq(const TokenSeq& tseq) {
    out << prefix(tseq.begin) << indent();
    for (size_t i = tseq.begin; i < tseq.end; i++) {
        out << tokens.name(i) << ' ';
    }
    out << endl;
}

void HLexDumper::dump(TokenGroup group) {
    switch (group.type_id) {
        case TokenSeq::type_id: {
            dump_tokenseq(group.as<TokenSeq>());
            break;
        }
        case GroupSeq::type_id: {
            dump_groupseq(group.as<GroupSeq>());
            break;
        }
        case Structured::type_id: {
            dump_structured(group.as<Structured>());
            break;
        }
        default:
            assert(false);
    }
}

void HLexer::dump(std::ostream& out) {
    HLexDumper dumper(out, tokens());
    for (auto group : root.groups) {
        dumper.dump_groupseq(group.get());
    }
}

TokenSeq HLexer::parse_token_seq() {
    skip_space();

    size_t begin = pos_;
    while (not finished() and not at_delimiter_or_separator()) {
        advance(1);
    }
    size_t end = pos_;
    // Do _NOT_ advance past the stopping token (a delimiter or separator),
    // we need to look at it the  just after this function

    return {.begin = begin, .end = end};
}

void HLexer::skip_space() {
    while (not finished() && (at(ENDL) || at(LINE_COMMENT))) {
        advance(1);
    }
}

Ref<Structured> HLexer::parse_structured(TokenID open, TokenID close) {
    size_t begin = pos_;
    assert(at(open));
    advance(1);

    size_t end;

    TokenID               sep = NONE;
    GroupSeq              gseq;
    vector<Ref<GroupSeq>> groups;

    TokenSeq seq = parse_token_seq();
    while (not finished() and not at(close)) {
        if (seq.begin != seq.end) {
            if (gseq.elems.empty()) {
                // Copy the begin from the first sequence (to skip spaces)
                gseq.begin = seq.begin;
            }
            gseq.elems.push_back(TokenGroup(TokenSeq::make(seq)));
        }
        if (finished()) {
            string tname = token_name(close);
            LinCol lc = tokens().token_line_col(pos_);
            error("{}:{}:{}: EOF while expecting '{}'", tokens().filename_, lc.lin, lc.col, tname);
        } else if (at_close_delimiter()) {
            // Wrong delimiter
            string tname = token_name(close);
            LinCol lincol = tokens().token_line_col(pos_);
            error("{}:{}:{}: Expected '{}'", tokens().filename_, lincol.lin, lincol.col, tname);
        } else if (at_separator()) {
            if (sep != NONE) {
                // FIXME(pauek): Handle two different separators
                if (curr() != sep) {
                    auto   tname = token_name(sep);
                    LinCol lincol = tokens().token_line_col(pos_);
                    error(
                        "{}:{}:{}: Expected '{}'", tokens().filename_, lincol.lin, lincol.col, tname
                    );
                }
            } else {
                sep = curr();
            }
            gseq.end = pos_;
            advance(1);
            groups.push_back(GroupSeq::make(gseq));
            gseq = GroupSeq();
        } else if (at_open_delimiter()) {
            auto open_ = curr();
            auto elem = parse_structured(open_, closing(open_));
            if (gseq.elems.empty()) {
                gseq.begin = elem.get().begin;
            }
            gseq.elems.push_back(TokenGroup(elem));

            // Special case: A block ({}) acts like a separator [funcdecl, for, while, if...]
            if (open_ == OPEN_BRACE) {
                groups.push_back(GroupSeq::make(gseq));
                gseq = GroupSeq();
            }

        } else {
            // We should not be here since `parse_token_group` should have
            // stopped at delimiter or separator
            assert(false);
        }
        seq = parse_token_seq();
    }
    if (seq.begin != seq.end) {
        if (gseq.elems.empty()) {
            gseq.begin = seq.begin;
        }
        gseq.elems.push_back(TokenGroup(TokenSeq::make(seq)));
    }
    if (not gseq.elems.empty()) {
        gseq.end = pos_;
        groups.push_back(GroupSeq::make(gseq));
    }

    assert(at(close));
    advance(1);

    end = pos_;

    return Structured::make({
        .delim = open,
        .sep = sep,
        .begin = begin,
        .end = end,
        .groups = groups,
    });
}

void HLexer::run() {
    auto& tokens = lexer_.tokens();

    GroupSeq gseq;
    while (not finished()) {
        TokenID tok = tokens.ids[pos_];
        switch (tok) {
            case SEMICOLON: {
                // This is a semicolon at the top level: consume it.
                advance(1);
                if (not gseq.elems.empty()) {
                    gseq.end = pos_;
                    root.groups.push_back(GroupSeq::make(gseq));
                    gseq = GroupSeq();
                }
                continue;
            }
            case OPEN_BRACKET: {
                auto group = parse_structured(OPEN_BRACKET, CLOSE_BRACKET);
                if (gseq.elems.empty()) {
                    gseq.begin = group.get().begin;
                }
                gseq.elems.push_back(TokenGroup(group));
                break;
            }
            case OPEN_PAREN: {
                auto group = parse_structured(OPEN_PAREN, CLOSE_PAREN);
                if (gseq.elems.empty()) {
                    gseq.begin = group.get().begin;
                }
                gseq.elems.push_back(TokenGroup(group));
                break;
            }
            case OPEN_BRACE: {
                auto group = parse_structured(OPEN_BRACE, CLOSE_BRACE);
                if (gseq.elems.empty()) {
                    gseq.begin = group.get().begin;
                }
                gseq.elems.push_back(TokenGroup(group));

                // A block acts like a separator
                if (not gseq.elems.empty()) {
                    gseq.end = pos_;
                    root.groups.push_back(GroupSeq::make(gseq));
                    gseq = GroupSeq();
                }
                break;
            }
            default: {
                auto seq = parse_token_seq();
                if (seq.begin != seq.end) {
                    if (gseq.elems.empty()) {
                        // Copy the begin from the first sequence (to skip spaces)
                        gseq.begin = seq.begin;
                    }
                    gseq.elems.push_back(TokenGroup(TokenSeq::make(seq)));
                }
            }
        }
    }
}
