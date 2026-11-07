#include <filesystem>
#include <iostream>
using namespace std;

#include "../astdump.hh"
#include "../lexer.hh"
#include "../parser.hh"
#include "../printer.hh"
#include "../utils.hh"
#include "command.hh"

struct ParseCmd : Command {
    ParseCmd(Args args) : Command(args) {}

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

    int parse_text(string text, string filename) {
        try {
            Lexer lexer(text, filename);
            lexer.run();

            Parser parser(lexer);
            auto   program = parser.run();

            AstDumper ast_dumper(cout, parser);
            ast_dumper.dump(program.get());

            return 0;

        } catch (Error *e) {
            e->print(cerr);
            return 1;
        }
    }

    int parse_file(string filename) {
        string text;
        if (not read_file(text, filename)) {
            return 1;
        }
        show_filename(filename);
        return parse_text(text, filename);
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
                ParseCmd::help();
                return nullptr;
            }
            return new ParseCmd(args);
        }
    };
};
