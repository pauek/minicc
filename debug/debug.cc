
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
int lineheight;
int program_width = 150;
const int bar_width = 5;

const Color COLOR_PROGRAM_BACKGROUND = (Color){ 235, 235, 255, 255 };
const Color COLOR_BAR                = (Color) { 100, 100, 100, 50 };
const Color COLOR_BAR_HOVER          = (Color) { 0, 0, 0, 50 };

bool inBar(int x) {
   const int margin = 2;
   return x >= program_width - bar_width - margin 
       && x <= program_width + margin;
}

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
      int H = lineheight;
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
      const char *fname;
      int ip = -1;
      vector<string> names;
      vector<Value>  values;

      Frame(const char *s, int n) : fname(s), ip(n) {}
   }; 
   vector<Frame> frames;
   Value result;

   Stack() {}

   void push(const char *fname, int n) { 
      frames.push_back(Frame(fname, n)); 
   }

   void pop() { frames.pop_back(); }
   Frame& top() { return frames.back(); }
   const Frame& top() const { return frames.back(); }
};

struct Highlight { int line, begin, end; };

struct Instr;
struct Program;
typedef void (*InstrFunc)(Program& P, Instr& I);

struct Instr { 
   InstrFunc fn; 
   Highlight hl;
};

struct Program {
   Stack stack;

    int curr() const { return stack.top().ip; }
   void next(int n)  { stack.top().ip = n; }

   bool end()  const { return stack.frames.empty(); }

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

   void push(const char* fname, int n) { stack.push(fname, n); }
   void pop()                          { stack.pop(); }

   void draw_source();
   void draw();

   void exec_one() {
      int ip = curr();
      (*instrs[ip].fn)(*this, instrs[ip]);
   }

   static Instr instrs[];
   static const char *source;
};

const char *getline(const char **str) {
   static char line[1024];
   char *t = line;
   const char *s = *str;
   while (*s != 0 && *s != '\n') {
      *t++ = *s++;
   }
   *t = 0;
   if (*s == '\n') s++;
   *str = s;
   return line;
}

void Program::draw_source() {
   Vector2 pos, size;

   // Draw highlight
   if (!end()) {
      int ip = stack.top().ip;
      Highlight H = instrs[ip].hl;
      if (H.line != -1) {
         pos.x  = 20 + 7.1 * (H.begin - 1) - 1;
         pos.y  = (H.line - 1) * (lineheight) + 4;
         size.x = 7.1 * (H.end - H.begin);
         size.y = lineheight;
         DrawRectangleV(pos, size, GREEN);
      }
   }

   // Draw source
   pos = { 20, 20 };
   const char *text = source;
   while (*text != 0) {
      const char *line = getline(&text);
      DrawTextEx(font, line, pos, font.baseSize, 0, BLACK);
      pos.y += lineheight;
   }
}

void Program::draw() {
   Vector2 pos;

   // Draw Stack
   pos = { program_width + 20.0f, screenHeight - 30 };
   if (!stack.frames.empty()) {
      DrawLine(pos.x, pos.y, pos.x + 200, pos.y, BLUE);
   }
   for (int i = 0; i < stack.frames.size(); i++) {
      Stack::Frame& f = stack.frames[i];
      pos.x += 20;
      for (size_t i = 0; i < f.names.size(); i++) {
         pos.y -= 30;
         DrawTextEx(font, f.names[i].c_str(), pos, font.baseSize, 0, BLACK);
         pos.x += 30;
         f.values[i].draw(pos);
         pos.x -= 30;
      }
      pos.y -= 30;
      DrawTextEx(font, f.fname, pos, font.baseSize, 0, BLACK);
      pos.x -= 20;
      pos.y -= 30;
      DrawLine(pos.x, pos.y, pos.x + 200, pos.y, BLUE);
   }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

const char *Program::source = R"(
int f(int a, int b) {
   int c = a + b;
   c++;
   return c;
}
   
int main() {
   cout << f(1, 2) << endl;
}
)";

// f(1, 2)
void main1(Program& P, Instr& I) {
   P.next(1);
   P.push("f", 2);
   P.add_local("a", Value(1));
   P.add_local("b", Value(2));
}

// cout << [result] << endl;
void main2(Program& P, Instr& I) {
   P.next(5);
   int result = P.result().get_int();
   // TODO: Emulate the output!
   cout << result << endl;
}

void main3(Program& P, Instr& I) {
   P.pop();
}

// int c = a + b;
void f1(Program& P, Instr& I) {
   int a = P.local(0).get_int();
   int b = P.local(1).get_int();
   int c = a + b;
   P.add_local("c", Value(c));
   P.next(3);
}

// c++;
void f2(Program& P, Instr& I) {
   P.local(2).set_int(P.local(2).get_int() + 1);
   P.next(4);
}

// return c;
void f3(Program& P, Instr& I) {
   P.result().set_int(P.local(2).get_int());
   P.pop(); // Esta vuelta está mal, debería ser la siguiente instrucción de la 0 después del 'call'
}

Instr Program::instrs[] = {
   { main1, { 9, 12, 19 } },
   { main2, { 9, 4, 28 } },
   { f1, { 3, 4, 18 } },
   { f2, { 4, 4, 8 } },
   { f3, { 5, 4, 13 } },
   { main3, {} },
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main() {
   InitWindow(screenWidth, screenHeight, "bola");
   SetConfigFlags(FLAG_VSYNC_HINT);
   SetTargetFPS(60);
   font = LoadSpriteFontEx("iosevka-regular.ttf", 18, 0, 0);
   lineheight = font.baseSize + 3;

   int orig_x = -1, orig_program_width;
   bool dragging_bar = false;

   Program P;
   P.push("main", 0);

   RenderTexture2D program_source = LoadRenderTexture(screenWidth, screenHeight);
   while (!WindowShouldClose())
   {
      BeginTextureMode(program_source);
         DrawRectangle(0, 0, screenWidth, screenHeight, COLOR_PROGRAM_BACKGROUND);
         P.draw_source();
         Color color_bar = COLOR_BAR;
         if (inBar(GetMouseX())) {
            color_bar = COLOR_BAR_HOVER;
         }
         DrawRectangle(program_width - bar_width, 0, program_width, screenHeight, color_bar);
      EndTextureMode();

      BeginDrawing();
         DrawTextureRec(program_source.texture, 
                        // WARNING: aquí hace falta poner     -screenHeight,    sino sale al revés
                        (Rectangle){ 0, 0, program_width + 1, -screenHeight },  
                        (Vector2){ 0, 0 }, 
                        WHITE);
         P.draw();
      EndDrawing();

      if (IsKeyPressed(KEY_SPACE) && !P.end()) {
         P.exec_one();
         if (P.end()) {
            break;
         }
      }

      // Drag bar 
      if (inBar(GetMouseX()) && IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
         dragging_bar = true;
         orig_x = GetMouseX();
         orig_program_width = program_width;
      }
      if (dragging_bar) {
         if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) {
            int new_width = orig_program_width + (GetMouseX() - orig_x); 
            if (new_width >= bar_width) {
               program_width = new_width;
            }
         } else {
            dragging_bar = false;
         }
      }
   }
   CloseWindow();
   return 0;
}
