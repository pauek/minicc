#include "walker.hh"
#include <sstream>
#include "ast.hh"
using namespace std;

struct ErrorCollector {
	std::vector<Error *>& errors;

	ErrorCollector(std::vector<Error *>& v) : errors(v) {}

	void walk(Ast *n) {
		const std::vector<Error *>& ve = n->errors;
		errors.insert(errors.end(), ve.begin(), ve.end());
		n->errors.clear();
	}
};

void collect_errors(Ast *ast, std::vector<Error *>& v) {
	if (ast == 0) {
		return;
	}
	ErrorCollector error_collector(v);
	walk(ast, error_collector);
	for (int i = 0; i < v.size(); i++) {
		if (v[i]->stopper) {
			v.resize(i + 1);
			break;
		}
	}
}
