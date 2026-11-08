#include <filesystem>
#include <iostream>
using namespace std;

#include "../astdump.hh"
#include "../hl-lexer.hh"
#include "../lexer.hh"
#include "../parser2.hh"
#include "../utils.hh"
#include "command.hh"

struct Parse2Cmd : Command {
    Parse2Cmd(Args args) : Command(args) {}

    bool read_file(string& text, string filename) {
        try {
            text = ::read_file(filename);
            return true;
        } catch (filesystem::filesystem_error& e) {
            cerr << "Could not read file '" << filename << "': " << e.what() << endl;
            return false;
        }
    }

    void show_filename(string filename) {
        // Do not show if there is only one
        if (args().size() > 1) {
            cout << filename << ":" << endl;
        }
    }

    int parse_file(string filename) {
        string text;
        if (not read_file(text, filename)) {
            return 1;
        }

        if (num_args() > 1) {
            cout << filename << ":" << endl;
        }

        Lexer lexer(text, filename);
        lexer.run();

        HLexer hl_lex(lexer);
        hl_lex.run();

        Parser2 parser2(hl_lex);
        auto    program = parser2.run();

        if (parser2.has_errors()) {
            for (auto& err : parser2.errors()) {
                string codeline = lexer.tokens().get_line(err.pos.lin);
                print_error(cerr, filename, err.pos, err.message, codeline);
            }
            return 1;
        } else {
            AstDumper dumper(cout, hl_lex.tokens());
            dumper.dump(program.get());
            return 0;
        }
    }

    int run() {
        for (auto filename : args()) {
            int ret = parse_file(filename);
            if (ret != 0) {
                return ret;
            }
        }
        return 0;
    }

    static constexpr const char *parse_usage =
        "Usage: minicc parse [options...] [file]...\n\n"
        "Shows the AST from file(s).\n\n"
        "Options:\n"
        "    none for now... ;)\n"
        "\n";

    static void help() { cout << parse_usage; }

    struct ArgParser : Command::ArgParser {
        ICommand *parse(Args args) override {
            if (args.empty()) {
                Parse2Cmd::help();
                return nullptr;
            }
            return new Parse2Cmd(args);
        }
    };
};
