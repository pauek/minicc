#include "command.hh"

#include <iostream>
#include <vector>

#include "help.hh"
#include "hlex.hh"
#include "lex.hh"
#include "parse.hh"
#include "parse2.hh"
#include "show-config.hh"
using namespace std;

size_t Command::max_name_len() {
    size_t mx = 0;
    for (auto cmd : table) {
        mx = max(mx, cmd.name.size());
    }
    return mx;
}

Command::Info *Command::find(string name) {
    for (int i = 0; i < Command::table.size(); i++) {
        if (Command::table[i].name == name) {
            return &Command::table[i];
        }
    }
    return nullptr;
}

ICommand *Command::ArgParser::parse(Args args) {
    Help::ArgParser help;
    if (args.empty()) {
        return help.parse(args);
    }
    auto pinfo = find(args[0]);
    if (pinfo == nullptr) {
        cerr << "Unknown command '" << args[0] << "'" << endl;
        return help.parse(args);
    }
    return pinfo->parser->parse(args.from(1));
}

// Add commands here:
vector<Command::Info> Command::table = {
    {"help",   "Show help",                    new Help::ArgParser()      },
    {"config", "Show configuration",           new ShowConfig::ArgParser()},
    {"lex",    "Lex file",                     new Lex::ArgParser()       },
    {"hlex",   "High-level Lex file",          new HLex::ArgParser()      },
    {"parse",  "Parse file and show the AST",  new ParseCmd::ArgParser()  },
    {"parse2", "Parse2 file and show the AST", new Parse2Cmd::ArgParser() },
};
