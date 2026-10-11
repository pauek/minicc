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

#define COMMAND(cmd_name) \
	int cmd_name(const std::vector<std::string>& args);

COMMAND(Help)
COMMAND(Lex)
COMMAND(Parse)

#endif
