#ifndef COMMAND_HH
#define COMMAND_HH

#include <string>
#include <vector>

struct ICommand {
	virtual int run() = 0;
};

template <auto CmdFn>
class Command : public ICommand {
    std::vector<std::string> args_;

   public:
    Command(const std::vector<std::string>& args) : args_(args) {}

    int run() override { return CmdFn(args_); }
};

int Help(const std::vector<std::string>& args);
int Lex(const std::vector<std::string>& args);

#endif
