#include <iostream>
using namespace std;

#include "hl-lexer.hh"

void HLLexer::dump(std::ostream& out){
	out << lexer_.tokens().ids.size() << endl;
}
