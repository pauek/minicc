
#include <iostream>
#include <fstream>
#include <sstream>
using namespace std;

// #include <emscripten.h>
#include <emscripten/bind.h>

#include "parser.hh"
#include "walker.hh"
#include "prettypr.hh"
#include "stepper.hh"
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

string reformat(string code) {
   istringstream in(code);
   Parser P(&in);
   AstNode *prog = P.parse();
   
   ostringstream out;
   PrettyPrinter pr(&out);
   prog->visit(&pr);
   return out.str();
}

class EmbindStepper {
   Stepper *S;
public:
   EmbindStepper() {
      S = new Stepper(&cin, &cout);
      program->visit(S);
   }
   string status()   const { return S->status(); }
    Range span()     const { return S->span(); }
     bool finished() const { return S->finished(); }
     void step()           { S->step(); }
   string env()      const { return S->env2json(); }
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
      .function("status",   &EmbindStepper::status)
      .function("span",     &EmbindStepper::span)
      .function("finished", &EmbindStepper::finished)
      .function("step",     &EmbindStepper::step)
      .function("env",      &EmbindStepper::env);
}

