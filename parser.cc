
#include "parser.h"
using namespace std;

AstNode* Parser::parse() {
   AstNode *res = new Program();
   while (_in.next()) {
      char c = _in.curr();
      cout << _in.pos() << " -> '" << _in.curr() << "'" << endl;
   }
   return res;
}
