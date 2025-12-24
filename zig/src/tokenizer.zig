// MiniCC Tokenizer - adapted from tokenizer_example.zig for MiniCC C++ subset

const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .keyword_and },
        .{ "or", .keyword_or },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
        .{ "for", .keyword_for },
        .{ "while", .keyword_while },
        .{ "break", .keyword_break },
        .{ "continue", .keyword_continue },
        .{ "goto", .keyword_goto },
        .{ "return", .keyword_return },
        .{ "switch", .keyword_switch },
        .{ "typedef", .keyword_typedef },
        .{ "class", .keyword_class },
        .{ "struct", .keyword_struct },
        .{ "enum", .keyword_enum },
        .{ "using", .keyword_using },
        .{ "namespace", .keyword_namespace },
        .{ "void", .keyword_void },
        .{ "bool", .keyword_bool },
        .{ "char", .keyword_char },
        .{ "int", .keyword_int },
        .{ "double", .keyword_double },
        .{ "float", .keyword_float },
        .{ "string", .keyword_string },
        .{ "signed", .keyword_signed },
        .{ "unsigned", .keyword_unsigned },
        .{ "volatile", .keyword_volatile },
        .{ "const", .keyword_const },
        .{ "short", .keyword_short },
        .{ "long", .keyword_long },
        .{ "inline", .keyword_inline },
        .{ "virtual", .keyword_virtual },
        .{ "explicit", .keyword_explicit },
        .{ "auto", .keyword_auto },
        .{ "register", .keyword_register },
        .{ "static", .keyword_static },
        .{ "extern", .keyword_extern },
        .{ "mutable", .keyword_mutable },
        .{ "true", .keyword_true },
        .{ "false", .keyword_false },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const Tag = enum {
        invalid,
        identifier,
        int_literal,
        char_literal,
        string_literal,
        float_literal,
        double_literal,
        eof,

        // Punctuation
        l_paren, // (
        r_paren, // )
        l_bracket, // [
        r_bracket, // ]
        l_brace, // {
        r_brace, // }
        semicolon, // ;
        comma, // ,
        dot, // .
        colon, // :
        colon_colon, // ::
        question_mark, // ?
        sharp, // #

        // Operators
        bang, // !
        bang_equal, // !=
        equal, // =
        equal_equal, // ==
        plus, // +
        plus_plus, // ++
        plus_equal, // +=
        minus, // -
        minus_minus, // --
        minus_equal, // -=
        arrow, // ->
        asterisk, // *
        asterisk_equal, // *=
        slash, // /
        slash_equal, // /=
        percent, // %
        percent_equal, // %=
        ampersand, // &
        ampersand_ampersand, // &&
        ampersand_equal, // &=
        pipe, // |
        pipe_pipe, // ||
        pipe_equal, // |=
        caret, // ^
        caret_equal, // ^=
        tilde, // ~
        angle_bracket_left, // <
        angle_bracket_left_equal, // <=
        angle_bracket_angle_bracket_left, // <<
        angle_bracket_angle_bracket_left_equal, // <<=
        angle_bracket_right, // >
        angle_bracket_right_equal, // >=
        angle_bracket_angle_bracket_right, // >>
        angle_bracket_angle_bracket_right_equal, // >>=

        // Keywords
        keyword_and,
        keyword_or,
        keyword_if,
        keyword_else,
        keyword_for,
        keyword_while,
        keyword_break,
        keyword_continue,
        keyword_goto,
        keyword_return,
        keyword_switch,
        keyword_typedef,
        keyword_class,
        keyword_struct,
        keyword_enum,
        keyword_using,
        keyword_namespace,
        keyword_void,
        keyword_bool,
        keyword_char,
        keyword_int,
        keyword_double,
        keyword_float,
        keyword_string,
        keyword_signed,
        keyword_unsigned,
        keyword_volatile,
        keyword_const,
        keyword_short,
        keyword_long,
        keyword_inline,
        keyword_virtual,
        keyword_explicit,
        keyword_auto,
        keyword_register,
        keyword_static,
        keyword_extern,
        keyword_mutable,
        keyword_true,
        keyword_false,

        pub fn lexeme(tag: Tag) ?[]const u8 {
            return switch (tag) {
                .invalid,
                .identifier,
                .int_literal,
                .char_literal,
                .string_literal,
                .float_literal,
                .double_literal,
                .eof,
                => null,

                .l_paren => "(",
                .r_paren => ")",
                .l_bracket => "[",
                .r_bracket => "]",
                .l_brace => "{",
                .r_brace => "}",
                .semicolon => ";",
                .comma => ",",
                .dot => ".",
                .colon => ":",
                .colon_colon => "::",
                .question_mark => "?",
                .sharp => "#",
                .bang => "!",
                .bang_equal => "!=",
                .equal => "=",
                .equal_equal => "==",
                .plus => "+",
                .plus_plus => "++",
                .plus_equal => "+=",
                .minus => "-",
                .minus_minus => "--",
                .minus_equal => "-=",
                .arrow => "->",
                .asterisk => "*",
                .asterisk_equal => "*=",
                .slash => "/",
                .slash_equal => "/=",
                .percent => "%",
                .percent_equal => "%=",
                .ampersand => "&",
                .ampersand_ampersand => "&&",
                .ampersand_equal => "&=",
                .pipe => "|",
                .pipe_pipe => "||",
                .pipe_equal => "|=",
                .caret => "^",
                .caret_equal => "^=",
                .tilde => "~",
                .angle_bracket_left => "<",
                .angle_bracket_left_equal => "<=",
                .angle_bracket_angle_bracket_left => "<<",
                .angle_bracket_angle_bracket_left_equal => "<<=",
                .angle_bracket_right => ">",
                .angle_bracket_right_equal => ">=",
                .angle_bracket_angle_bracket_right => ">>",
                .angle_bracket_angle_bracket_right_equal => ">>=",
                .keyword_and => "and",
                .keyword_or => "or",
                .keyword_if => "if",
                .keyword_else => "else",
                .keyword_for => "for",
                .keyword_while => "while",
                .keyword_break => "break",
                .keyword_continue => "continue",
                .keyword_goto => "goto",
                .keyword_return => "return",
                .keyword_switch => "switch",
                .keyword_typedef => "typedef",
                .keyword_class => "class",
                .keyword_struct => "struct",
                .keyword_enum => "enum",
                .keyword_using => "using",
                .keyword_namespace => "namespace",
                .keyword_void => "void",
                .keyword_bool => "bool",
                .keyword_char => "char",
                .keyword_int => "int",
                .keyword_double => "double",
                .keyword_float => "float",
                .keyword_string => "string",
                .keyword_signed => "signed",
                .keyword_unsigned => "unsigned",
                .keyword_volatile => "volatile",
                .keyword_const => "const",
                .keyword_short => "short",
                .keyword_long => "long",
                .keyword_inline => "inline",
                .keyword_virtual => "virtual",
                .keyword_explicit => "explicit",
                .keyword_auto => "auto",
                .keyword_register => "register",
                .keyword_static => "static",
                .keyword_extern => "extern",
                .keyword_mutable => "mutable",
                .keyword_true => "true",
                .keyword_false => "false",
            };
        }

        pub fn symbol(tag: Tag) []const u8 {
            return tag.lexeme() orelse switch (tag) {
                .invalid => "invalid token",
                .identifier => "an identifier",
                .int_literal => "an integer literal",
                .char_literal => "a character literal",
                .string_literal => "a string literal",
                .float_literal => "a float literal",
                .double_literal => "a double literal",
                .eof => "EOF",
                else => unreachable,
            };
        }
    };
};

pub const Tokenizer = struct {
    buffer: [:0]const u8,
    index: usize,

    /// For debugging purposes.
    pub fn dump(self: *Tokenizer, token: *const Token) void {
        std.debug.print("{s} \"{s}\"\n", .{ @tagName(token.tag), self.buffer[token.loc.start..token.loc.end] });
    }

    pub fn init(buffer: [:0]const u8) Tokenizer {
        // Skip the UTF-8 BOM if present.
        return .{
            .buffer = buffer,
            .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0,
        };
    }

    const State = enum {
        start,
        invalid,
        identifier,
        string_literal,
        string_literal_backslash,
        char_literal,
        char_literal_backslash,
        int,
        int_period,
        float,
        float_exponent,
        equal,
        bang,
        plus,
        minus,
        asterisk,
        slash,
        line_comment,
        multi_line_comment,
        multi_line_comment_asterisk,
        percent,
        ampersand,
        pipe,
        caret,
        angle_bracket_left,
        angle_bracket_angle_bracket_left,
        angle_bracket_right,
        angle_bracket_angle_bracket_right,
        colon,
        period,
        saw_u_or_l,
        tilde,
    };

    /// After this returns invalid, it will reset on the next newline, returning tokens starting from there.
    /// An eof token will always be returned at the end.
    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        state: switch (State.start) {
            .start => switch (self.buffer[self.index]) {
                0 => {
                    if (self.index == self.buffer.len) {
                        return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    } else {
                        continue :state .invalid;
                    }
                },
                ' ', '\n', '\t', '\r' => {
                    self.index += 1;
                    result.loc.start = self.index;
                    continue :state .start;
                },
                '"' => {
                    result.tag = .string_literal;
                    self.index += 1;
                    continue :state .string_literal;
                },
                '\'' => {
                    result.tag = .char_literal;
                    self.index += 1;
                    continue :state .char_literal;
                },
                'u', 'U', 'l', 'L' => {
                    // Could be identifier or prefix for string/char literal
                    continue :state .saw_u_or_l;
                },
                '=' => continue :state .equal,
                '!' => continue :state .bang,
                '+' => continue :state .plus,
                '-' => continue :state .minus,
                '*' => continue :state .asterisk,
                '/' => continue :state .slash,
                '%' => continue :state .percent,
                '&' => continue :state .ampersand,
                '|' => continue :state .pipe,
                '^' => continue :state .caret,
                '~' => continue :state .tilde,
                '<' => continue :state .angle_bracket_left,
                '>' => continue :state .angle_bracket_right,
                ':' => continue :state .colon,
                '.' => continue :state .period,
                '(' => {
                    result.tag = .l_paren;
                    self.index += 1;
                },
                ')' => {
                    result.tag = .r_paren;
                    self.index += 1;
                },
                '[' => {
                    result.tag = .l_bracket;
                    self.index += 1;
                },
                ']' => {
                    result.tag = .r_bracket;
                    self.index += 1;
                },
                '{' => {
                    result.tag = .l_brace;
                    self.index += 1;
                },
                '}' => {
                    result.tag = .r_brace;
                    self.index += 1;
                },
                ';' => {
                    result.tag = .semicolon;
                    self.index += 1;
                },
                ',' => {
                    result.tag = .comma;
                    self.index += 1;
                },
                '?' => {
                    result.tag = .question_mark;
                    self.index += 1;
                },
                '#' => {
                    result.tag = .sharp;
                    self.index += 1;
                },
                '0'...'9' => {
                    result.tag = .int_literal;
                    self.index += 1;
                    continue :state .int;
                },
                else => {
                    // Check if it's a letter or underscore (identifier)
                    // All other special characters have been handled above
                    const ch = self.buffer[self.index];
                    if ((ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z') or ch == '_') {
                        result.tag = .identifier;
                        continue :state .identifier;
                    } else {
                        continue :state .invalid;
                    }
                },
            },

            .invalid => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },

            .saw_u_or_l => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '"' => {
                        result.tag = .string_literal;
                        self.index += 1;
                        continue :state .string_literal;
                    },
                    '\'' => {
                        result.tag = .char_literal;
                        self.index += 1;
                        continue :state .char_literal;
                    },
                    '0'...'9' => {
                        // It's L123 or similar - treat as identifier followed by number
                        // Actually, we need to backtrack - but let's just treat as identifier
                        result.tag = .identifier;
                        continue :state .identifier;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        // It's an identifier
                        result.tag = .identifier;
                        continue :state .identifier;
                    },
                    else => {
                        // Just the character itself (u, U, l, or L as identifier)
                        result.tag = .identifier;
                        const ident = self.buffer[result.loc.start..self.index];
                        if (Token.getKeyword(ident)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },

            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const ident = self.buffer[result.loc.start..self.index];
                        if (Token.getKeyword(ident)) |tag| {
                            result.tag = tag;
                        }
                    },
                }
            },

            .string_literal => {
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => {
                        self.index += 1;
                        continue :state .string_literal_backslash;
                    },
                    '"' => self.index += 1,
                    else => {
                        self.index += 1;
                        continue :state .string_literal;
                    },
                }
            },

            .string_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .string_literal,
                }
            },

            .char_literal => {
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '\n' => result.tag = .invalid,
                    '\\' => {
                        self.index += 1;
                        continue :state .char_literal_backslash;
                    },
                    '\'' => self.index += 1,
                    else => {
                        self.index += 1;
                        continue :state .char_literal;
                    },
                }
            },

            .char_literal_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0, '\n' => result.tag = .invalid,
                    else => continue :state .char_literal,
                }
            },

            .equal => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .equal_equal;
                        self.index += 1;
                    },
                    else => result.tag = .equal,
                }
            },

            .bang => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .bang_equal;
                        self.index += 1;
                    },
                    else => result.tag = .bang,
                }
            },

            .plus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '+' => {
                        result.tag = .plus_plus;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .plus_equal;
                        self.index += 1;
                    },
                    else => result.tag = .plus,
                }
            },

            .minus => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-' => {
                        result.tag = .minus_minus;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .minus_equal;
                        self.index += 1;
                    },
                    '>' => {
                        result.tag = .arrow;
                        self.index += 1;
                    },
                    else => result.tag = .minus,
                }
            },

            .asterisk => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .asterisk_equal;
                        self.index += 1;
                    },
                    else => result.tag = .asterisk,
                }
            },

            .slash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '/' => continue :state .line_comment,
                    '*' => {
                        self.index += 1;
                        continue :state .multi_line_comment;
                    },
                    '=' => {
                        result.tag = .slash_equal;
                        self.index += 1;
                    },
                    else => result.tag = .slash,
                }
            },

            .line_comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else return .{
                            .tag = .eof,
                            .loc = .{
                                .start = self.index,
                                .end = self.index,
                            },
                        };
                    },
                    '\n' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    else => continue :state .line_comment,
                }
            },

            .multi_line_comment => {
                switch (self.buffer[self.index]) {
                    0 => {
                        if (self.index != self.buffer.len) {
                            continue :state .invalid;
                        } else {
                            result.tag = .invalid;
                        }
                    },
                    '*' => {
                        self.index += 1;
                        continue :state .multi_line_comment_asterisk;
                    },
                    else => {
                        self.index += 1;
                        continue :state .multi_line_comment;
                    },
                }
            },

            .multi_line_comment_asterisk => {
                switch (self.buffer[self.index]) {
                    '/' => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '*' => {
                        self.index += 1;
                        continue :state .multi_line_comment_asterisk;
                    },
                    else => {
                        self.index += 1;
                        continue :state .multi_line_comment;
                    },
                }
            },

            .percent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .percent_equal;
                        self.index += 1;
                    },
                    else => result.tag = .percent,
                }
            },

            .ampersand => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '&' => {
                        result.tag = .ampersand_ampersand;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .ampersand_equal;
                        self.index += 1;
                    },
                    else => result.tag = .ampersand,
                }
            },

            .pipe => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '|' => {
                        result.tag = .pipe_pipe;
                        self.index += 1;
                    },
                    '=' => {
                        result.tag = .pipe_equal;
                        self.index += 1;
                    },
                    else => result.tag = .pipe,
                }
            },

            .caret => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .caret_equal;
                        self.index += 1;
                    },
                    else => result.tag = .caret,
                }
            },

            .tilde => {
                self.index += 1;
                result.tag = .tilde;
            },

            .angle_bracket_left => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '<' => continue :state .angle_bracket_angle_bracket_left,
                    '=' => {
                        result.tag = .angle_bracket_left_equal;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_left,
                }
            },

            .angle_bracket_angle_bracket_left => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .angle_bracket_angle_bracket_left_equal;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_angle_bracket_left,
                }
            },

            .angle_bracket_right => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '>' => continue :state .angle_bracket_angle_bracket_right,
                    '=' => {
                        result.tag = .angle_bracket_right_equal;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_right,
                }
            },

            .angle_bracket_angle_bracket_right => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        result.tag = .angle_bracket_angle_bracket_right_equal;
                        self.index += 1;
                    },
                    else => result.tag = .angle_bracket_angle_bracket_right,
                }
            },

            .colon => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .colon_colon;
                        self.index += 1;
                    },
                    else => result.tag = .colon,
                }
            },

            .period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        result.tag = .double_literal;
                        continue :state .float;
                    },
                    else => result.tag = .dot,
                }
            },

            .int => switch (self.buffer[self.index]) {
                '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                '.' => continue :state .int_period,
                'e', 'E' => {
                    self.index += 1;
                    continue :state .float_exponent;
                },
                'f', 'F' => {
                    result.tag = .float_literal;
                    self.index += 1;
                },
                'u', 'U', 'l', 'L' => {
                    // Integer suffix - skip it
                    self.index += 1;
                    // Could have multiple suffixes like UL, ULL, etc.
                    while (self.index < self.buffer.len) : (self.index += 1) {
                        switch (self.buffer[self.index]) {
                            'u', 'U', 'l', 'L' => continue,
                            else => break,
                        }
                    }
                },
                else => {},
            },

            .int_period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '0'...'9' => {
                        result.tag = .double_literal;
                        self.index += 1;
                        continue :state .float;
                    },
                    'e', 'E' => {
                        result.tag = .double_literal;
                        self.index += 1;
                        continue :state .float_exponent;
                    },
                    'f', 'F' => {
                        result.tag = .float_literal;
                        self.index += 1;
                    },
                    'l', 'L' => {
                        result.tag = .double_literal;
                        self.index += 1;
                    },
                    else => {
                        // It was just a number followed by a dot - this is a valid float literal (e.g., 123.)
                        result.tag = .double_literal;
                        // Don't backtrack - the period is part of the float literal
                    },
                }
            },

            .float => switch (self.buffer[self.index]) {
                '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                'e', 'E' => {
                    self.index += 1;
                    continue :state .float_exponent;
                },
                'f', 'F' => {
                    result.tag = .float_literal;
                    self.index += 1;
                },
                'l', 'L' => {
                    // Long double suffix
                    result.tag = .double_literal;
                    self.index += 1;
                },
                else => {
                    if (result.tag == .invalid) {
                        result.tag = .double_literal;
                    }
                },
            },

            .float_exponent => {
                // Set tag to double_literal when we enter exponent state
                result.tag = .double_literal;
                switch (self.buffer[self.index]) {
                    '+', '-' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    '0'...'9' => {
                        continue :state .float;
                    },
                    else => {
                        // Invalid exponent
                        result.tag = .invalid;
                    },
                }
            },
        }

        result.loc.end = self.index;
        return result;
    }
};

// Tests
test "single punctuation tokens" {
    const input = "()[]{};,";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.l_paren, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_bracket, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_bracket, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "single operators" {
    const input = "+-*/%%= =!&|^<>";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.plus, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.minus, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.asterisk, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.slash, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.percent, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.percent_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.bang, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.ampersand, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.pipe, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.caret, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_left, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_right, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "two-character operators" {
    const input = "== != ++ -- += -= *= /= %= && || << >>";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.equal_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.bang_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.plus_plus, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.minus_minus, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.plus_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.minus_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.asterisk_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.slash_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.percent_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.ampersand_ampersand, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.pipe_pipe, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_angle_bracket_left, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_angle_bracket_right, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "keywords" {
    const input = "if else for while return int void";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.keyword_if, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_else, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_for, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_while, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_return, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_int, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_void, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "identifiers" {
    const input = "hello world _underscore var123";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "integer literals" {
    const input = "0 42 123 999";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "simple expression" {
    const input = "x = 42;";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "if statement" {
    const input = "if (x == 0) return;";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.keyword_if, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.equal_equal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_return, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "whitespace handling" {
    const input = "  x  \t  y  \n  z  ";
    var tokenizer = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, tokenizer.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "float literal - simple period" {
    const input = ".123";
    var tokenizer = Tokenizer.init(input);
    const token = tokenizer.next();
    std.debug.print("Token tag: {s}, lexeme: {s}\n", .{ @tagName(token.tag), input[token.loc.start..token.loc.end] });
    try std.testing.expectEqual(Token.Tag.double_literal, token.tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

fn testFloatLiteral(comptime input: [:0]const u8, expected_tag: Token.Tag) !void {
    var tokenizer = Tokenizer.init(input);
    const token = tokenizer.next();
    try std.testing.expectEqual(expected_tag, token.tag);
    try std.testing.expectEqual(Token.Tag.eof, tokenizer.next().tag);
}

test "float literals - all combinations" {
    // Test cases: (input, expected_tag)
    const test_cases = [_]struct { input: [:0]const u8, expected_tag: Token.Tag }{
        // Starting with period (no leading digits)
        .{ .input = ".123", .expected_tag = .double_literal },
        .{ .input = ".456", .expected_tag = .double_literal },
        .{ .input = ".0", .expected_tag = .double_literal },
        .{ .input = ".123f", .expected_tag = .float_literal },
        .{ .input = ".456F", .expected_tag = .float_literal },
        .{ .input = ".0f", .expected_tag = .float_literal },
        .{ .input = ".123L", .expected_tag = .double_literal },
        .{ .input = ".456l", .expected_tag = .double_literal },
        .{ .input = ".0L", .expected_tag = .double_literal },

        // Period with exponent
        .{ .input = ".123e10", .expected_tag = .double_literal },
        .{ .input = ".456E10", .expected_tag = .double_literal },
        .{ .input = ".123e+10", .expected_tag = .double_literal },
        .{ .input = ".456e-10", .expected_tag = .double_literal },
        .{ .input = ".123E+10", .expected_tag = .double_literal },
        .{ .input = ".456E-10", .expected_tag = .double_literal },
        .{ .input = ".0e5", .expected_tag = .double_literal },
        .{ .input = ".0E-5", .expected_tag = .double_literal },

        // Period with exponent and suffix
        .{ .input = ".123e10f", .expected_tag = .float_literal },
        .{ .input = ".456E10F", .expected_tag = .float_literal },
        .{ .input = ".123e+10f", .expected_tag = .float_literal },
        .{ .input = ".456e-10F", .expected_tag = .float_literal },
        .{ .input = ".123E+10f", .expected_tag = .float_literal },
        .{ .input = ".456E-10F", .expected_tag = .float_literal },
        .{ .input = ".123e10L", .expected_tag = .double_literal },
        .{ .input = ".456E10l", .expected_tag = .double_literal },
        .{ .input = ".123e+10L", .expected_tag = .double_literal },
        .{ .input = ".456e-10l", .expected_tag = .double_literal },

        // Integer followed by period (with fractional part)
        .{ .input = "123.456", .expected_tag = .double_literal },
        .{ .input = "42.0", .expected_tag = .double_literal },
        .{ .input = "0.123", .expected_tag = .double_literal },
        .{ .input = "999.999", .expected_tag = .double_literal },
        .{ .input = "123.456f", .expected_tag = .float_literal },
        .{ .input = "42.0F", .expected_tag = .float_literal },
        .{ .input = "0.123f", .expected_tag = .float_literal },
        .{ .input = "999.999F", .expected_tag = .float_literal },
        .{ .input = "123.456L", .expected_tag = .double_literal },
        .{ .input = "42.0l", .expected_tag = .double_literal },
        .{ .input = "0.123L", .expected_tag = .double_literal },
        .{ .input = "999.999l", .expected_tag = .double_literal },

        // Integer followed by period (no fractional part)
        .{ .input = "123.", .expected_tag = .double_literal },
        .{ .input = "42.", .expected_tag = .double_literal },
        .{ .input = "0.", .expected_tag = .double_literal },
        .{ .input = "123.f", .expected_tag = .float_literal },
        .{ .input = "42.F", .expected_tag = .float_literal },
        .{ .input = "0.f", .expected_tag = .float_literal },
        .{ .input = "123.L", .expected_tag = .double_literal },
        .{ .input = "42.l", .expected_tag = .double_literal },
        .{ .input = "0.L", .expected_tag = .double_literal },

        // Integer with exponent (no period)
        .{ .input = "123e10", .expected_tag = .double_literal },
        .{ .input = "42E10", .expected_tag = .double_literal },
        .{ .input = "0e5", .expected_tag = .double_literal },
        .{ .input = "123e+10", .expected_tag = .double_literal },
        .{ .input = "42E+10", .expected_tag = .double_literal },
        .{ .input = "0e+5", .expected_tag = .double_literal },
        .{ .input = "123e-10", .expected_tag = .double_literal },
        .{ .input = "42E-10", .expected_tag = .double_literal },
        .{ .input = "0e-5", .expected_tag = .double_literal },

        // Integer with exponent and suffix (no period)
        .{ .input = "123e10f", .expected_tag = .float_literal },
        .{ .input = "42E10F", .expected_tag = .float_literal },
        .{ .input = "0e5f", .expected_tag = .float_literal },
        .{ .input = "123e+10f", .expected_tag = .float_literal },
        .{ .input = "42E+10F", .expected_tag = .float_literal },
        .{ .input = "0e+5f", .expected_tag = .float_literal },
        .{ .input = "123e-10f", .expected_tag = .float_literal },
        .{ .input = "42E-10F", .expected_tag = .float_literal },
        .{ .input = "0e-5f", .expected_tag = .float_literal },
        .{ .input = "123e10L", .expected_tag = .double_literal },
        .{ .input = "42E10l", .expected_tag = .double_literal },
        .{ .input = "0e5L", .expected_tag = .double_literal },
        .{ .input = "123e+10L", .expected_tag = .double_literal },
        .{ .input = "42E+10l", .expected_tag = .double_literal },
        .{ .input = "0e+5L", .expected_tag = .double_literal },
        .{ .input = "123e-10L", .expected_tag = .double_literal },
        .{ .input = "42E-10l", .expected_tag = .double_literal },
        .{ .input = "0e-5L", .expected_tag = .double_literal },

        // Integer + period + exponent
        .{ .input = "123.456e10", .expected_tag = .double_literal },
        .{ .input = "42.0E10", .expected_tag = .double_literal },
        .{ .input = "0.123e5", .expected_tag = .double_literal },
        .{ .input = "123.456e+10", .expected_tag = .double_literal },
        .{ .input = "42.0E+10", .expected_tag = .double_literal },
        .{ .input = "0.123e+5", .expected_tag = .double_literal },
        .{ .input = "123.456e-10", .expected_tag = .double_literal },
        .{ .input = "42.0E-10", .expected_tag = .double_literal },
        .{ .input = "0.123e-5", .expected_tag = .double_literal },

        // Integer + period (no fractional) + exponent
        .{ .input = "123.e10", .expected_tag = .double_literal },
        .{ .input = "42.E10", .expected_tag = .double_literal },
        .{ .input = "0.e5", .expected_tag = .double_literal },
        .{ .input = "123.e+10", .expected_tag = .double_literal },
        .{ .input = "42.E+10", .expected_tag = .double_literal },
        .{ .input = "0.e+5", .expected_tag = .double_literal },
        .{ .input = "123.e-10", .expected_tag = .double_literal },
        .{ .input = "42.E-10", .expected_tag = .double_literal },
        .{ .input = "0.e-5", .expected_tag = .double_literal },

        // Integer + period + exponent + suffix
        .{ .input = "123.456e10f", .expected_tag = .float_literal },
        .{ .input = "42.0E10F", .expected_tag = .float_literal },
        .{ .input = "0.123e5f", .expected_tag = .float_literal },
        .{ .input = "123.456e+10f", .expected_tag = .float_literal },
        .{ .input = "42.0E+10F", .expected_tag = .float_literal },
        .{ .input = "0.123e+5f", .expected_tag = .float_literal },
        .{ .input = "123.456e-10f", .expected_tag = .float_literal },
        .{ .input = "42.0E-10F", .expected_tag = .float_literal },
        .{ .input = "0.123e-5f", .expected_tag = .float_literal },
        .{ .input = "123.456e10L", .expected_tag = .double_literal },
        .{ .input = "42.0E10l", .expected_tag = .double_literal },
        .{ .input = "0.123e5L", .expected_tag = .double_literal },
        .{ .input = "123.456e+10L", .expected_tag = .double_literal },
        .{ .input = "42.0E+10l", .expected_tag = .double_literal },
        .{ .input = "0.123e+5L", .expected_tag = .double_literal },
        .{ .input = "123.456e-10L", .expected_tag = .double_literal },
        .{ .input = "42.0E-10l", .expected_tag = .double_literal },
        .{ .input = "0.123e-5L", .expected_tag = .double_literal },

        // Integer + period (no fractional) + exponent + suffix
        .{ .input = "123.e10f", .expected_tag = .float_literal },
        .{ .input = "42.E10F", .expected_tag = .float_literal },
        .{ .input = "0.e5f", .expected_tag = .float_literal },
        .{ .input = "123.e+10f", .expected_tag = .float_literal },
        .{ .input = "42.E+10F", .expected_tag = .float_literal },
        .{ .input = "0.e+5f", .expected_tag = .float_literal },
        .{ .input = "123.e-10f", .expected_tag = .float_literal },
        .{ .input = "42.E-10F", .expected_tag = .float_literal },
        .{ .input = "0.e-5f", .expected_tag = .float_literal },
        .{ .input = "123.e10L", .expected_tag = .double_literal },
        .{ .input = "42.E10l", .expected_tag = .double_literal },
        .{ .input = "0.e5L", .expected_tag = .double_literal },
        .{ .input = "123.e+10L", .expected_tag = .double_literal },
        .{ .input = "42.E+10l", .expected_tag = .double_literal },
        .{ .input = "0.e+5L", .expected_tag = .double_literal },
        .{ .input = "123.e-10L", .expected_tag = .double_literal },
        .{ .input = "42.E-10l", .expected_tag = .double_literal },
        .{ .input = "0.e-5L", .expected_tag = .double_literal },

        // Edge cases with multiple digits in exponent
        .{ .input = "1.23e123", .expected_tag = .double_literal },
        .{ .input = "4.56E-456", .expected_tag = .double_literal },
        .{ .input = "7.89e+789", .expected_tag = .double_literal },
        .{ .input = "1.23e123f", .expected_tag = .float_literal },
        .{ .input = "4.56E-456F", .expected_tag = .float_literal },
        .{ .input = "7.89e+789L", .expected_tag = .double_literal },

        // Very small and very large numbers
        .{ .input = "1e-10", .expected_tag = .double_literal },
        .{ .input = "1E+10", .expected_tag = .double_literal },
        .{ .input = "1.5e-20", .expected_tag = .double_literal },
        .{ .input = "1.5E+20", .expected_tag = .double_literal },
        .{ .input = "1e-10f", .expected_tag = .float_literal },
        .{ .input = "1E+10F", .expected_tag = .float_literal },
        .{ .input = "1.5e-20f", .expected_tag = .float_literal },
        .{ .input = "1.5E+20F", .expected_tag = .float_literal },
    };

    inline for (test_cases) |case| {
        try testFloatLiteral(case.input, case.expected_tag);
    }
}
