
#include <iostream>
#include <fstream>
#include <sstream>
using namespace std;

// #include <emscripten.h>
#include <emscripten/bind.h>

#include "parser.hh"
#include "walker.hh"
#include "interpreter.hh"

AstNode *program;

string compile(string code) {
   istringstream S(code);
   Parser P(&S);
   program = P.parse();

   ostringstream E;
   vector<Error*> errors;
   collect_errors(program, errors);
   for (Error *e : errors) {
      E << e->msg << endl;
   }
   return E.str();
}

string execute(string input) {
   istringstream in(input);
   ostringstream out;
   try {
      Interpreter I(&in, &out);
      program->visit(&I);
   } 
   catch (EvalError* e) {
      ofstream errors("/minicc/errors");
      errors <<  e->msg << endl;
   }
   return out.str();
}

EMSCRIPTEN_BINDINGS(minicc) {
   emscripten::function("compile", &compile);
   emscripten::function("execute", &execute);
}

