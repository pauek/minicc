
/*

Intentamos escribir un programa que sea equivalente a:

   int f(int a, int b) {
      int c = a + b;
      c++;
      return c;
   }
   
   int main() {
      cout << f(1, 2) << endl;
   }

Pero que contenga toda la información para poder debugarlo

*/

#include <assert.h>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

enum Type { NONE, INTEGER, FLOAT };

struct Value {
   Type type;
   union {
      int as_int;
      float as_float;
   };

   Value()        : type(NONE)               {}
   Value(int x)   : type(INTEGER), as_int(x) {}
   Value(float f) : type(FLOAT), as_float(f) {}

     int get_int()    const { assert(type == INTEGER); return as_int; }
   float get_float()  const { assert(type == FLOAT);   return as_float; }

   void set_int(int x) { 
      if (type == NONE) {
         type = INTEGER;
      } else if (type != INTEGER) {
         assert(false);
      }
      as_int = x; 
   }

   void set_float(float x) { assert(type == FLOAT);   as_float = x; }

   string as_string() const {
      ostringstream oss;
      switch (type) {
      case NONE:    oss << "none" << endl; break;
      case INTEGER: oss << "int = " << as_int; break;
      case FLOAT:   oss << "float = " << as_float; break;
      }
      return oss.str();
   }
};


struct Stack 
{
   struct Frame {
      vector<string> names;
      vector<Value>  values;
      Value result;
      Frame *below = 0;
   }; 
   Frame *top, *bottom;

   Stack() { bottom = top = new Frame; }
};

struct Context {
   Stack stack;

   int add_local(string name, Value v) { 
      assert(stack.top->names.size() == stack.top->values.size());
      int idx = stack.top->names.size();
      stack.top->names.push_back(name);
      stack.top->values.push_back(v);
      return idx;
   }

   Value& local(int index) { return stack.top->values[index]; }
   Value& result()         { return stack.top->result; }

   void push() {
      Stack::Frame *new_frame = new Stack::Frame;
      new_frame->below = stack.top;
      stack.top = new_frame;
   }

   void pop() {
      Stack::Frame *new_top = stack.top->below;
      delete stack.top;
      stack.top = new_top;
   }

   void intermediate_value(int x)   {}
   void intermediate_value(float x) {}

   void show() {
      Stack::Frame *f = stack.top;
      while (f != 0) {
         cout << "Frame: " << f << endl;
         for (size_t i = 0; i < f->names.size(); i++) {
            cout << f->names[i] << ' ' << f->values[i].as_string() << endl;
         }
         f = f->below;
      }
      cout << endl;
   }
};

struct Instruction;
typedef int (*InstrFunc)(Context& C, Instruction& I);

struct Instruction { 
   InstrFunc fn; 
};

int f0(Context& C, Instruction& I) {
   C.push();
   C.add_local("a", Value(1));
   C.add_local("b", Value(2));
   return 1;
}

int f1(Context& C, Instruction& I) {
   int a = C.local(0).get_int();
   int b = C.local(1).get_int();
   int c = a + b;
   C.intermediate_value(c);
   C.add_local("c", Value(c));
   return 2;
}

int f2(Context& C, Instruction& I) {
   C.local(2).set_int(C.local(2).get_int() + 1);
   return 3;
}

int f3(Context& C, Instruction& I) {
   C.result().set_int(C.local(2).get_int());
   return 4; // Esta vuelta está mal, debería ser la siguiente instrucción de la 0 después del 'call'
}

int f4(Context& C, Instruction& I) {
   int result = C.result().get_int();
   C.pop();
   cout << result << endl;
   return -1;
}

Instruction instructions[] = {
   { f0 },
   { f1 },
   { f2 },
   { f3 },
   { f4 },
};

int main() {
   Context C;
   C.add_local("x", Value(7.5f));
   int i = 0;
   while (i != -1) {
      C.show();
      i = (*instructions[i].fn)(C, instructions[i]);
   }
}
