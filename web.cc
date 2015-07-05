
#include <iostream>
#include <fstream>
#include <sstream>
using namespace std;

// #include <emscripten.h>
#include <emscripten/bind.h>

#include "parser.hh"
#include "semantic.hh"
#include "walker.hh"
#include "prettypr.hh"
#include "interpreter.hh"
#include "stepper.hh"
#include "translator.hh"

AstNode *program;

string print_errors(const vector<Error*>& errors) {
   ostringstream E;
   E << "[";
   bool first = true;
   for (const Error *e : errors) {
      if (!first) E << ", " << endl;
      e->to_json(E);
      first = false;
      // delete e; // ??
   }
   E << "]";
   return E.str();
}

string compile(string code) {
   istringstream S(code);
   Parser P(&S);
   program = P.parse();

   vector<Error*> errors;

   // parse errors
   collect_errors(program, errors);
   if (!errors.empty()) {
      return print_errors(errors);
   }

   // semantic errors
   SemanticAnalyzer A;
   program->accept(&A);
   collect_errors(program, errors);
   return print_errors(errors);
}

string execute(string input) {
   istringstream in(input);
   ostringstream out;
   try {
      Interpreter I(&in, &out);
      program->accept(&I);
   } 
   catch (Error* e) {
      ofstream errors("/minicc/errors");
      errors <<  e->msg << endl;
   }
   return out.str();
}

string reformat(string code) {
   istringstream in(code);
   Parser P(&in);
   AstNode *prog = P.parse();
   
   ostringstream out;
   PrettyPrinter pr(&out);
   prog->accept(&pr);
   return out.str();
}

class EmbindStepper {
   Stepper *S;
public:
   EmbindStepper() {
      Translator::translator.set_language("ca");
      S = new Stepper(&cin, &cout);
      program->accept(S);
   }
     bool finished() const { return S->finished(); }
     bool step()           { return S->step(); }
   string state()    const { return S->state2json(); }
   string output()   const { return S->output(); }
   string error()    const { return S->error()->msg; }
};

EMSCRIPTEN_BINDINGS(minicc) {
   emscripten::function("compile", &compile);
   emscripten::function("execute", &execute);
   emscripten::function("reformat", &reformat);
   emscripten::class_<Pos>("Pos")
      .property("lin", &Pos::l, &Pos::sl)
      .property("col", &Pos::c, &Pos::sc);
   emscripten::class_<Range>("Range")
      .property("ini", &Range::gi, &Range::si)
      .property("fin", &Range::gf, &Range::sf);
   emscripten::class_<EmbindStepper>("Stepper")
      .constructor<>()
      .function("finished", &EmbindStepper::finished)
      .function("step",     &EmbindStepper::step)
      .function("state",    &EmbindStepper::state)
      .function("output",   &EmbindStepper::output)
      .function("error",    &EmbindStepper::error);
}

