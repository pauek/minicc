
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

#include "raylib.h"

#include <assert.h>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

const int screenWidth = 1280;
const int screenHeight = 960;

SpriteFont font;

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

   void draw(Vector2 pos) {
      int H = font.baseSize + 5;
      switch (type) {
         case NONE: break;
         case INTEGER: {
            ostringstream oss;
            oss << as_int;
            DrawRectangleLines(pos.x - 4, pos.y - H + 5, 100, H, RED);
            DrawTextEx(font, oss.str().c_str(), pos, font.baseSize, 0, BLACK);
            break;
         }
         case FLOAT: {
            ostringstream oss;
            oss << as_float;
            DrawRectangleLines(pos.x - 4, pos.y - H + 5, 100, H, RED);
            DrawTextEx(font, oss.str().c_str(), pos, font.baseSize, 0, BLACK);
            break;
         }
      }
   }
};


struct Stack 
{
   struct Frame {
      vector<string> names;
      vector<Value>  values;
      Frame *below = 0;
   }; 
   vector<Frame> frames;
   Value result;

   Stack() {}

   void push() { frames.push_back(Frame()); }
   void pop()  { frames.pop_back(); }

   Frame& top() { return frames.back(); }
};

struct Context {
   Stack stack;
   vector<int> ip; // instruction pointer

    int curr() const { return ip.back(); }
   void next(int n)  { ip.back() = n; }

   bool end()  const { return ip.empty(); }

   int add_local(string name, Value v) {
      Stack::Frame& curr_frame = stack.top();
      assert(curr_frame.names.size() == curr_frame.values.size());
      int idx = curr_frame.names.size();
      curr_frame.names.push_back(name);
      curr_frame.values.push_back(v);
      return idx;
   }

   Value& local(int index) { return stack.top().values[index]; }
   Value& result()         { return stack.result; }

   void push(int n) {
      stack.push();
      ip.push_back(n);
   }

   void pop() {
      stack.pop();
      ip.pop_back();
   }

   void intermediate_value(int x)   {}
   void intermediate_value(float x) {}

   void draw() {
      Vector2 pos = { 50, screenHeight - 30 };
      for (int i = 0; i < stack.frames.size(); i++) {
         Stack::Frame& f = stack.frames[i];
         DrawLine(pos.x, pos.y, pos.x + 200, pos.y, BLUE);
         pos.x += 20;
         for (size_t i = 0; i < f.names.size(); i++) {
            pos.y -= 30;
            DrawTextEx(font, f.names[i].c_str(), pos, font.baseSize, 0, BLACK);
            pos.x += 30;
            f.values[i].draw(pos);
            pos.x -= 30;
         }
         pos.x -= 20;
         pos.y -= 40;
      }
      DrawLine(pos.x, pos.y, pos.x + 200, pos.y, BLUE);
   }
};

struct Instr;
typedef void (*InstrFunc)(Context& C, Instr& I);

struct Instr { 
   InstrFunc fn; 
};

// f(1, 2)
void f0(Context& C, Instr& I) {
   C.next(4);
   C.push(1);
   C.add_local("a", Value(1));
   C.add_local("b", Value(2));
}

// int c = a + b;
void f1(Context& C, Instr& I) {
   int a = C.local(0).get_int();
   int b = C.local(1).get_int();
   int c = a + b;
   C.intermediate_value(c);
   C.add_local("c", Value(c));
   C.next(2);
}

// c++;
void f2(Context& C, Instr& I) {
   C.local(2).set_int(C.local(2).get_int() + 1);
   C.next(3);
}

// return c;
void f3(Context& C, Instr& I) {
   C.result().set_int(C.local(2).get_int());
   C.pop(); // Esta vuelta está mal, debería ser la siguiente instrucción de la 0 después del 'call'
}

// cout << [result] << endl;
void f4(Context& C, Instr& I) {
   int result = C.result().get_int();
   cout << result << endl;
   C.pop();
}

Instr instructions[] = {
   { f0 },
   { f1 },
   { f2 },
   { f3 },
   { f4 },
};

int main() {
   InitWindow(screenWidth, screenHeight, "bola");
   SetConfigFlags(FLAG_VSYNC_HINT);
   SetTargetFPS(60);
   font = LoadSpriteFontTTF("iosevka-regular.ttf", 24, 0, 0);

   Context C;
   C.push(0);
   C.add_local("x", Value(7.5f));

   while (!WindowShouldClose())
   {
      BeginDrawing();
         // DrawTextEx(font, "hi, there!", (Vector2){400, 300}, font.baseSize, 0, BLACK);
         C.draw();
      EndDrawing();

      if (IsKeyPressed(KEY_SPACE) && !C.end()) {
         int i = C.curr();
         (*instructions[i].fn)(C, instructions[i]);
      }
   }
   CloseWindow();
   return 0;
}
