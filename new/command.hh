#ifndef COMMAND_HH
#define COMMAND_HH

#include <cassert>
#include <iostream>
#include <string>
#include <vector>

struct ICommand {
    virtual int run() = 0;
};

class Args {
    typedef std::vector<std::string> Container;

    Container args_;
    size_t    from_;

   public:
    Args(int argc, char *argv[]) : args_(argv, argv + argc), from_(0) {}
    Args(const Args& other) : args_(other.args_), from_(other.from_) {}

    Args(const Args& other, size_t from) : args_(other.args_), from_(other.from_ + from) {}

    bool   empty() const { return from_ >= args_.size(); }
    size_t size() const { return empty() ? 0 : args_.size() - from_; }

    Container::const_iterator begin() const {
        return (empty() ? args_.end() : args_.begin() + from_);
    }
    Container::const_iterator end() const { return args_.end(); }

    Args from(size_t from) { return Args(*this, from); }

    std::string operator[](size_t i) {
        assert(from_ + i < args_.size());
        return args_[from_ + i];
    }
};

template <auto CmdFn>
class Command : public ICommand {
    Args args_;

   public:
    Command(Args args) : args_(args) {}

    int run() override { return CmdFn(args_); }
};

template <auto CmdFn>
ICommand *parse_args(Args args) {
    // Handle subcommands if necessary...
    return new Command<CmdFn>(args);
}

#define COMMAND(cmd_name) int cmd_name(Args args);

COMMAND(Help)
COMMAND(Lex)
COMMAND(Parse)

// If we want to make subcommands in Parse, we can implement parse_args<Parse>...
template <>
ICommand *parse_args<Parse>(Args args);

#endif
