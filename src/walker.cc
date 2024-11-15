#include "walker.hh"
#include <sstream>
#include "ast.hh"
using namespace std;

struct ErrorCollector {
    std::vector<Error *>& errors;

    ErrorCollector(std::vector<Error *>& v) : errors(v) {}

    void walk(AstNode *n) {
        const std::vector<Error *>& ve = n->errors;
        errors.insert(errors.end(), ve.begin(), ve.end());
        n->errors.clear();
    }
};

std::vector<Error *> collect_errors(AstNode *node) {
    std::vector<Error *> result;
    if (node == 0) {
        return result;
    }
    walk(node, ErrorCollector(result));
    for (int i = 0; i < result.size(); i++) {
        if (result[i]->stopper) {
            result.resize(i + 1);
            break;
        }
    }
    return result;
}
