#ifndef COMMAND_HH
#define COMMAND_HH

#include <algorithm>
#include <cassert>
#include <string>
#include <vector>

// The command interface is just the `run` function, which returns something
// similar to the `main` function, i.e. an error code (0 = success, etc.)
struct ICommand {
    virtual int run() = 0;
};

// Args implements a slice of the arguments of a program (stored in a vector)
// with a starting index (`start_`) different than 0, but no end index
// (Peeling layers like that is typical of cmd/subcmd/subsubcmd hierarchies)
class Args {
    const size_t argc_, start_;
    const char **argv_;

   public:
    Args(int argc, const char *argv[]) : argc_(argc), start_(0), argv_(argv) {}

    Args(const Args& A, size_t start) : argc_(A.argc_), start_(A.start_ + start), argv_(A.argv_) {
        assert(start_ <= argc_);
    }
    Args from(size_t from) { return Args(*this, from); }

    bool   empty() const { return start_ >= argc_; }
    size_t size() const { return argc_ - start_; }

    std::string operator[](size_t i) {
        assert(start_ + i < argc_);
        return argv_[start_ + i];
    }

    typedef const char **const_iterator;

    const_iterator end() const { return argv_ + argc_; }
    const_iterator begin() const { return argv_ + std::min(argc_, start_); }
};

typedef int (*CmdFuncPointer)(Args args);

class Command : public ICommand {
    Args args_;

   public:
    Command(Args args) : args_(args) {}

    size_t num_args() const { return args_.size(); }
    Args&  args() { return args_; }

    struct ArgParser {
        virtual ICommand *parse(Args args);
    };

    // Command Table

    struct Info {
        std::string name, help;
        ArgParser  *parser;
    };
    static std::vector<Info> table;

    static size_t max_name_len();
    static Info  *find(std::string name);
};

#endif
