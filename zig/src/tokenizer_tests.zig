const std = @import("std");
const tokenizer = @import("tokenizer.zig");
const Tokenizer = tokenizer.Tokenizer;
const Token = tokenizer.Token;

// Tests
test "single punctuation tokens" {
    const input = "()[]{};,";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.l_paren, T.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, T.next().tag);
    try std.testing.expectEqual(Token.Tag.l_bracket, T.next().tag);
    try std.testing.expectEqual(Token.Tag.r_bracket, T.next().tag);
    try std.testing.expectEqual(Token.Tag.l_brace, T.next().tag);
    try std.testing.expectEqual(Token.Tag.r_brace, T.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, T.next().tag);
    try std.testing.expectEqual(Token.Tag.comma, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "single operators" {
    const input = "+-*/%%= =!&|^<>";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.plus, T.next().tag);
    try std.testing.expectEqual(Token.Tag.minus, T.next().tag);
    try std.testing.expectEqual(Token.Tag.asterisk, T.next().tag);
    try std.testing.expectEqual(Token.Tag.slash, T.next().tag);
    try std.testing.expectEqual(Token.Tag.percent, T.next().tag);
    try std.testing.expectEqual(Token.Tag.percent_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.bang, T.next().tag);
    try std.testing.expectEqual(Token.Tag.ampersand, T.next().tag);
    try std.testing.expectEqual(Token.Tag.pipe, T.next().tag);
    try std.testing.expectEqual(Token.Tag.caret, T.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_left, T.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_right, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "two-character operators" {
    const input = "== != ++ -- += -= *= /= %= && || << >>";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.equal_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.bang_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.plus_plus, T.next().tag);
    try std.testing.expectEqual(Token.Tag.minus_minus, T.next().tag);
    try std.testing.expectEqual(Token.Tag.plus_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.minus_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.asterisk_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.slash_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.percent_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.ampersand_ampersand, T.next().tag);
    try std.testing.expectEqual(Token.Tag.pipe_pipe, T.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_angle_bracket_left, T.next().tag);
    try std.testing.expectEqual(Token.Tag.angle_bracket_angle_bracket_right, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "keywords" {
    const input = "if else for while return int void";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.keyword_if, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_else, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_for, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_while, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_return, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_int, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_void, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "identifiers" {
    const input = "hello world _underscore var123";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "integer literals" {
    const input = "0 42 123 999";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "simple expression" {
    const input = "x = 42;";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "if statement" {
    const input = "if (x == 0) return;";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.keyword_if, T.next().tag);
    try std.testing.expectEqual(Token.Tag.l_paren, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.equal_equal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.int_literal, T.next().tag);
    try std.testing.expectEqual(Token.Tag.r_paren, T.next().tag);
    try std.testing.expectEqual(Token.Tag.keyword_return, T.next().tag);
    try std.testing.expectEqual(Token.Tag.semicolon, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "whitespace handling" {
    const input = "  x  \t  y  \n  z  ";
    var T = Tokenizer.init(input);

    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.identifier, T.next().tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "float literal - simple period" {
    const input = ".123";
    var T = Tokenizer.init(input);
    const token = T.next();
    try std.testing.expectEqual(Token.Tag.double_literal, token.tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

fn testFloatLiteral(comptime input: [:0]const u8, expected_tag: Token.Tag) !void {
    var T = Tokenizer.init(input);
    const token = T.next();
    try std.testing.expectEqual(expected_tag, token.tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
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

fn testKeyword(comptime input: [:0]const u8, expected_tag: Token.Tag) !void {
    var T = Tokenizer.init(input);
    const token = T.next();
    try std.testing.expectEqual(expected_tag, token.tag);
    try std.testing.expectEqual(Token.Tag.eof, T.next().tag);
}

test "all keywords - positive and negative tests" {
    // Positive tests: all keywords should be recognized as keywords
    const positive_tests = [_]struct { input: [:0]const u8, expected_tag: Token.Tag }{
        .{ .input = "and", .expected_tag = .keyword_and },
        .{ .input = "or", .expected_tag = .keyword_or },
        .{ .input = "if", .expected_tag = .keyword_if },
        .{ .input = "else", .expected_tag = .keyword_else },
        .{ .input = "for", .expected_tag = .keyword_for },
        .{ .input = "while", .expected_tag = .keyword_while },
        .{ .input = "break", .expected_tag = .keyword_break },
        .{ .input = "continue", .expected_tag = .keyword_continue },
        .{ .input = "goto", .expected_tag = .keyword_goto },
        .{ .input = "return", .expected_tag = .keyword_return },
        .{ .input = "switch", .expected_tag = .keyword_switch },
        .{ .input = "typedef", .expected_tag = .keyword_typedef },
        .{ .input = "class", .expected_tag = .keyword_class },
        .{ .input = "struct", .expected_tag = .keyword_struct },
        .{ .input = "enum", .expected_tag = .keyword_enum },
        .{ .input = "using", .expected_tag = .keyword_using },
        .{ .input = "namespace", .expected_tag = .keyword_namespace },
        .{ .input = "void", .expected_tag = .keyword_void },
        .{ .input = "bool", .expected_tag = .keyword_bool },
        .{ .input = "char", .expected_tag = .keyword_char },
        .{ .input = "int", .expected_tag = .keyword_int },
        .{ .input = "double", .expected_tag = .keyword_double },
        .{ .input = "float", .expected_tag = .keyword_float },
        .{ .input = "string", .expected_tag = .keyword_string },
        .{ .input = "signed", .expected_tag = .keyword_signed },
        .{ .input = "unsigned", .expected_tag = .keyword_unsigned },
        .{ .input = "volatile", .expected_tag = .keyword_volatile },
        .{ .input = "const", .expected_tag = .keyword_const },
        .{ .input = "short", .expected_tag = .keyword_short },
        .{ .input = "long", .expected_tag = .keyword_long },
        .{ .input = "inline", .expected_tag = .keyword_inline },
        .{ .input = "virtual", .expected_tag = .keyword_virtual },
        .{ .input = "explicit", .expected_tag = .keyword_explicit },
        .{ .input = "auto", .expected_tag = .keyword_auto },
        .{ .input = "register", .expected_tag = .keyword_register },
        .{ .input = "static", .expected_tag = .keyword_static },
        .{ .input = "extern", .expected_tag = .keyword_extern },
        .{ .input = "mutable", .expected_tag = .keyword_mutable },
        .{ .input = "true", .expected_tag = .keyword_true },
        .{ .input = "false", .expected_tag = .keyword_false },
    };

    // Negative tests: words that start like keywords but aren't keywords should be identifiers
    const negative_tests = [_]struct { input: [:0]const u8 }{
        // Prefixes of keywords (incomplete)
        .{ .input = "i" },
        .{ .input = "el" },
        .{ .input = "els" },
        .{ .input = "fo" },
        .{ .input = "wh" },
        .{ .input = "whi" },
        .{ .input = "whil" },
        .{ .input = "br" },
        .{ .input = "bre" },
        .{ .input = "brea" },
        .{ .input = "co" },
        .{ .input = "con" },
        .{ .input = "cont" },
        .{ .input = "conti" },
        .{ .input = "contin" },
        .{ .input = "continu" },
        .{ .input = "go" },
        .{ .input = "got" },
        .{ .input = "re" },
        .{ .input = "ret" },
        .{ .input = "retu" },
        .{ .input = "retur" },
        .{ .input = "sw" },
        .{ .input = "swi" },
        .{ .input = "swit" },
        .{ .input = "switc" },
        .{ .input = "ty" },
        .{ .input = "typ" },
        .{ .input = "type" },
        .{ .input = "typed" },
        .{ .input = "typede" },
        .{ .input = "cl" },
        .{ .input = "cla" },
        .{ .input = "clas" },
        .{ .input = "st" },
        .{ .input = "str" },
        .{ .input = "stru" },
        .{ .input = "struc" },
        .{ .input = "en" },
        .{ .input = "enu" },
        .{ .input = "us" },
        .{ .input = "usi" },
        .{ .input = "usin" },
        .{ .input = "na" },
        .{ .input = "nam" },
        .{ .input = "name" },
        .{ .input = "names" },
        .{ .input = "namesp" },
        .{ .input = "namespa" },
        .{ .input = "namespac" },
        .{ .input = "vo" },
        .{ .input = "voi" },
        .{ .input = "bo" },
        .{ .input = "boo" },
        .{ .input = "ch" },
        .{ .input = "cha" },
        .{ .input = "in" },
        .{ .input = "do" },
        .{ .input = "dou" },
        .{ .input = "doub" },
        .{ .input = "doubl" },
        .{ .input = "fl" },
        .{ .input = "flo" },
        .{ .input = "floa" },
        .{ .input = "st" },
        .{ .input = "str" },
        .{ .input = "stri" },
        .{ .input = "strin" },
        .{ .input = "si" },
        .{ .input = "sig" },
        .{ .input = "sign" },
        .{ .input = "signe" },
        .{ .input = "un" },
        .{ .input = "uns" },
        .{ .input = "unsi" },
        .{ .input = "unsig" },
        .{ .input = "unsign" },
        .{ .input = "unsigne" },
        .{ .input = "unsignee" },
        .{ .input = "vo" },
        .{ .input = "vol" },
        .{ .input = "vola" },
        .{ .input = "volat" },
        .{ .input = "volati" },
        .{ .input = "volatil" },
        .{ .input = "co" },
        .{ .input = "con" },
        .{ .input = "cons" },
        .{ .input = "sh" },
        .{ .input = "sho" },
        .{ .input = "shor" },
        .{ .input = "lo" },
        .{ .input = "lon" },
        .{ .input = "vi" },
        .{ .input = "vir" },
        .{ .input = "virt" },
        .{ .input = "virtu" },
        .{ .input = "virtua" },
        .{ .input = "ex" },
        .{ .input = "explici" },
        .{ .input = "explic" },
        .{ .input = "expli" },
        .{ .input = "expl" },
        .{ .input = "exp" },
        .{ .input = "au" },
        .{ .input = "aut" },
        .{ .input = "re" },
        .{ .input = "reg" },
        .{ .input = "regi" },
        .{ .input = "regis" },
        .{ .input = "regist" },
        .{ .input = "registe" },
        .{ .input = "st" },
        .{ .input = "sta" },
        .{ .input = "stat" },
        .{ .input = "stati" },
        .{ .input = "ex" },
        .{ .input = "ext" },
        .{ .input = "exte" },
        .{ .input = "exter" },
        .{ .input = "mu" },
        .{ .input = "mut" },
        .{ .input = "muta" },
        .{ .input = "mutab" },
        .{ .input = "mutabl" },
        .{ .input = "tr" },
        .{ .input = "tru" },
        .{ .input = "fa" },
        .{ .input = "fal" },
        .{ .input = "fals" },
        // Words that start like keywords but have extra characters
        .{ .input = "ifx" },
        .{ .input = "iff" },
        .{ .input = "ifelse" },
        .{ .input = "elseif" },
        .{ .input = "elsex" },
        .{ .input = "forloop" },
        .{ .input = "forx" },
        .{ .input = "whileloop" },
        .{ .input = "whilex" },
        .{ .input = "breakpoint" },
        .{ .input = "breakx" },
        .{ .input = "continuex" },
        .{ .input = "gotox" },
        .{ .input = "returnx" },
        .{ .input = "switchx" },
        .{ .input = "typedefx" },
        .{ .input = "classx" },
        .{ .input = "structx" },
        .{ .input = "enumx" },
        .{ .input = "usingx" },
        .{ .input = "namespacex" },
        .{ .input = "voidx" },
        .{ .input = "boolx" },
        .{ .input = "charx" },
        .{ .input = "intx" },
        .{ .input = "doublex" },
        .{ .input = "floatx" },
        .{ .input = "stringx" },
        .{ .input = "signedx" },
        .{ .input = "unsignedx" },
        .{ .input = "volatilex" },
        .{ .input = "constx" },
        .{ .input = "shortx" },
        .{ .input = "longx" },
        .{ .input = "inlinex" },
        .{ .input = "virtualx" },
        .{ .input = "explicitx" },
        .{ .input = "autox" },
        .{ .input = "registerx" },
        .{ .input = "staticx" },
        .{ .input = "externx" },
        .{ .input = "mutablex" },
        .{ .input = "truex" },
        .{ .input = "falsex" },
        // Words with numbers
        .{ .input = "if1" },
        .{ .input = "else2" },
        .{ .input = "for3" },
        .{ .input = "while4" },
        .{ .input = "int5" },
        .{ .input = "void6" },
        // Words with underscores
        .{ .input = "if_" },
        .{ .input = "else_" },
        .{ .input = "for_" },
        .{ .input = "while_" },
        .{ .input = "int_" },
        .{ .input = "void_" },
        .{ .input = "_if" },
        .{ .input = "_else" },
        .{ .input = "_for" },
        .{ .input = "_while" },
        .{ .input = "_int" },
        .{ .input = "_void" },
        // Similar but different words
        .{ .input = "andx" },
        .{ .input = "orx" },
        .{ .input = "xand" },
        .{ .input = "xor" },
    };

    // Run positive tests
    inline for (positive_tests) |case| {
        try testKeyword(case.input, case.expected_tag);
    }

    // Run negative tests - these should all be identifiers
    inline for (negative_tests) |case| {
        try testKeyword(case.input, .identifier);
    }
}

test "token positions - comprehensive" {
    // Create a comprehensive input string with many different token types
    // We'll track positions manually to verify
    const input = "if (x == 42) { return \"hello\" + 'a' - 3.14; }";
    var T = Tokenizer.init(input);

    // Track expected positions as we parse
    var pos: usize = 0;

    // Helper function to check token position
    const checkToken = struct {
        fn check(
            token: Token,
            expected_tag: Token.Tag,
            expected_start: usize,
            expected_end: usize,
            expected_text: []const u8,
        ) !void {
            try std.testing.expectEqual(expected_tag, token.tag);
            try std.testing.expectEqual(expected_start, token.loc.start);
            try std.testing.expectEqual(expected_end, token.loc.end);
            const actual_text = input[token.loc.start..token.loc.end];
            try std.testing.expectEqualStrings(expected_text, actual_text);
        }
    }.check;

    // Skip whitespace at start
    pos = 0; // "if (x == 42) { return \"hello\" + 'a' - 3.14; }"
    var token = T.next(); // "if"
    try checkToken(token, .keyword_if, 0, 2, "if");
    pos = 3; // skip space after "if"

    token = T.next(); // "("
    try checkToken(token, .l_paren, 3, 4, "(");
    pos = 4;

    token = T.next(); // "x"
    try checkToken(token, .identifier, 4, 5, "x");
    pos = 6; // skip space after "x"

    token = T.next(); // "=="
    try checkToken(token, .equal_equal, 6, 8, "==");
    pos = 9; // skip space after "=="

    token = T.next(); // "42"
    try checkToken(token, .int_literal, 9, 11, "42");
    pos = 11;

    token = T.next(); // ")"
    try checkToken(token, .r_paren, 11, 12, ")");
    pos = 13; // skip space after ")"

    token = T.next(); // "{"
    try checkToken(token, .l_brace, 13, 14, "{");
    pos = 15; // skip space after "{"

    token = T.next(); // "return"
    try checkToken(token, .keyword_return, 15, 21, "return");
    pos = 22; // skip space after "return"

    token = T.next(); // "\"hello\""
    try checkToken(token, .string_literal, 22, 29, "\"hello\"");
    pos = 30; // skip space after "\"hello\""

    token = T.next(); // "+"
    try checkToken(token, .plus, 30, 31, "+");
    pos = 32; // skip space after "+"

    token = T.next(); // "'a'"
    try checkToken(token, .char_literal, 32, 35, "'a'");
    pos = 36; // skip space after "'a'"

    token = T.next(); // "-"
    try checkToken(token, .minus, 36, 37, "-");
    pos = 38; // skip space after "-"

    token = T.next(); // "3.14"
    try checkToken(token, .double_literal, 38, 42, "3.14");
    pos = 42;

    token = T.next(); // ";"
    try checkToken(token, .semicolon, 42, 43, ";");
    pos = 44; // skip space after ";"

    token = T.next(); // "}"
    try checkToken(token, .r_brace, 44, 45, "}");
    pos = 45;

    token = T.next(); // EOF
    try std.testing.expectEqual(Token.Tag.eof, token.tag);
    try std.testing.expectEqual(input.len, token.loc.start);
    try std.testing.expectEqual(input.len, token.loc.end);
}

test "token positions - operators and punctuation" {
    const input = "++ -- == != <= >= << >> <<= >>= += -= *= /= %= &= |= ^= && || -> ::";
    var T = Tokenizer.init(input);

    const TestCase = struct {
        tag: Token.Tag,
        expected_text: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .tag = .plus_plus, .expected_text = "++" },
        .{ .tag = .minus_minus, .expected_text = "--" },
        .{ .tag = .equal_equal, .expected_text = "==" },
        .{ .tag = .bang_equal, .expected_text = "!=" },
        .{ .tag = .angle_bracket_left_equal, .expected_text = "<=" },
        .{ .tag = .angle_bracket_right_equal, .expected_text = ">=" },
        .{ .tag = .angle_bracket_angle_bracket_left, .expected_text = "<<" },
        .{ .tag = .angle_bracket_angle_bracket_right, .expected_text = ">>" },
        .{ .tag = .angle_bracket_angle_bracket_left_equal, .expected_text = "<<=" },
        .{ .tag = .angle_bracket_angle_bracket_right_equal, .expected_text = ">>=" },
        .{ .tag = .plus_equal, .expected_text = "+=" },
        .{ .tag = .minus_equal, .expected_text = "-=" },
        .{ .tag = .asterisk_equal, .expected_text = "*=" },
        .{ .tag = .slash_equal, .expected_text = "/=" },
        .{ .tag = .percent_equal, .expected_text = "%=" },
        .{ .tag = .ampersand_equal, .expected_text = "&=" },
        .{ .tag = .pipe_equal, .expected_text = "|=" },
        .{ .tag = .caret_equal, .expected_text = "^=" },
        .{ .tag = .ampersand_ampersand, .expected_text = "&&" },
        .{ .tag = .pipe_pipe, .expected_text = "||" },
        .{ .tag = .arrow, .expected_text = "->" },
        .{ .tag = .colon_colon, .expected_text = "::" },
    };

    var pos: usize = 0;
    for (test_cases) |case| {
        const token = T.next();
        try std.testing.expectEqual(case.tag, token.tag);
        const actual_text = input[token.loc.start..token.loc.end];
        try std.testing.expectEqualStrings(case.expected_text, actual_text);
        try std.testing.expectEqual(pos, token.loc.start);
        pos = token.loc.end;
        // Skip whitespace
        while (pos < input.len and input[pos] == ' ') {
            pos += 1;
        }
    }

    const eof_token = T.next();
    try std.testing.expectEqual(Token.Tag.eof, eof_token.tag);
    try std.testing.expectEqual(input.len, eof_token.loc.start);
    try std.testing.expectEqual(input.len, eof_token.loc.end);
}

test "token positions - literals and identifiers" {
    const input = "hello world123 _underscore 42 0 999 3.14 2.5f .123 123e10 \"string\" 'c'";
    var T = Tokenizer.init(input);

    const TestCase = struct {
        tag: Token.Tag,
        expected_text: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .tag = .identifier, .expected_text = "hello" },
        .{ .tag = .identifier, .expected_text = "world123" },
        .{ .tag = .identifier, .expected_text = "_underscore" },
        .{ .tag = .int_literal, .expected_text = "42" },
        .{ .tag = .int_literal, .expected_text = "0" },
        .{ .tag = .int_literal, .expected_text = "999" },
        .{ .tag = .double_literal, .expected_text = "3.14" },
        .{ .tag = .float_literal, .expected_text = "2.5f" },
        .{ .tag = .double_literal, .expected_text = ".123" },
        .{ .tag = .double_literal, .expected_text = "123e10" },
        .{ .tag = .string_literal, .expected_text = "\"string\"" },
        .{ .tag = .char_literal, .expected_text = "'c'" },
    };

    var pos: usize = 0;
    for (test_cases) |case| {
        const token = T.next();
        try std.testing.expectEqual(case.tag, token.tag);
        const actual_text = input[token.loc.start..token.loc.end];
        try std.testing.expectEqualStrings(case.expected_text, actual_text);
        try std.testing.expectEqual(pos, token.loc.start);
        pos = token.loc.end;
        // Skip whitespace
        while (pos < input.len and input[pos] == ' ') {
            pos += 1;
        }
    }

    const eof_token = T.next();
    try std.testing.expectEqual(Token.Tag.eof, eof_token.tag);
    try std.testing.expectEqual(input.len, eof_token.loc.start);
    try std.testing.expectEqual(input.len, eof_token.loc.end);
}

test "token positions - keywords" {
    const input = "if else for while break continue return int void bool char double float";
    var T = Tokenizer.init(input);

    const TestCase = struct {
        tag: Token.Tag,
        expected_text: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .tag = .keyword_if, .expected_text = "if" },
        .{ .tag = .keyword_else, .expected_text = "else" },
        .{ .tag = .keyword_for, .expected_text = "for" },
        .{ .tag = .keyword_while, .expected_text = "while" },
        .{ .tag = .keyword_break, .expected_text = "break" },
        .{ .tag = .keyword_continue, .expected_text = "continue" },
        .{ .tag = .keyword_return, .expected_text = "return" },
        .{ .tag = .keyword_int, .expected_text = "int" },
        .{ .tag = .keyword_void, .expected_text = "void" },
        .{ .tag = .keyword_bool, .expected_text = "bool" },
        .{ .tag = .keyword_char, .expected_text = "char" },
        .{ .tag = .keyword_double, .expected_text = "double" },
        .{ .tag = .keyword_float, .expected_text = "float" },
    };

    var pos: usize = 0;
    for (test_cases) |case| {
        const token = T.next();
        try std.testing.expectEqual(case.tag, token.tag);
        const actual_text = input[token.loc.start..token.loc.end];
        try std.testing.expectEqualStrings(case.expected_text, actual_text);
        try std.testing.expectEqual(pos, token.loc.start);
        pos = token.loc.end;
        // Skip whitespace
        while (pos < input.len and input[pos] == ' ') {
            pos += 1;
        }
    }

    const eof_token = T.next();
    try std.testing.expectEqual(Token.Tag.eof, eof_token.tag);
    try std.testing.expectEqual(input.len, eof_token.loc.start);
    try std.testing.expectEqual(input.len, eof_token.loc.end);
}

test "token positions - punctuation" {
    const input = "()[]{};,.?:#";
    var T = Tokenizer.init(input);

    const TestCase = struct {
        tag: Token.Tag,
        expected_text: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .tag = .l_paren, .expected_text = "(" },
        .{ .tag = .r_paren, .expected_text = ")" },
        .{ .tag = .l_bracket, .expected_text = "[" },
        .{ .tag = .r_bracket, .expected_text = "]" },
        .{ .tag = .l_brace, .expected_text = "{" },
        .{ .tag = .r_brace, .expected_text = "}" },
        .{ .tag = .semicolon, .expected_text = ";" },
        .{ .tag = .comma, .expected_text = "," },
        .{ .tag = .dot, .expected_text = "." },
        .{ .tag = .question_mark, .expected_text = "?" },
        .{ .tag = .colon, .expected_text = ":" },
        .{ .tag = .sharp, .expected_text = "#" },
    };

    var pos: usize = 0;
    for (test_cases) |case| {
        const token = T.next();
        try std.testing.expectEqual(case.tag, token.tag);
        const actual_text = input[token.loc.start..token.loc.end];
        try std.testing.expectEqualStrings(case.expected_text, actual_text);
        try std.testing.expectEqual(pos, token.loc.start);
        pos = token.loc.end;
    }

    const eof_token = T.next();
    try std.testing.expectEqual(Token.Tag.eof, eof_token.tag);
    try std.testing.expectEqual(input.len, eof_token.loc.start);
    try std.testing.expectEqual(input.len, eof_token.loc.end);
}
