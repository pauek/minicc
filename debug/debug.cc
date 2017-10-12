
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
#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

const int screenWidth = 1280;
const int screenHeight = 960;

SpriteFont font;
SpriteFont font_bold;

int lineheight;
int program_width = 250;

const int min_program_width = 100;
const int bar_width = 5;
const int status_bar_height = 45;

Texture2D tx_buttons;

enum DraggingWhat { 
   DRAGGING_NOTHING,
   DRAGGING_STATUS_BAR_KNOB, 
   DRAGGING_BAR 
};
DraggingWhat dragging;

// Themes

struct Colors {
   Color highlight;
   Color program_background;
   Color bar, bar_hover;
   Color knob, knob_dragging;
   Color status_bar_background;
};

struct Theme {
   Colors colors;
};

Theme defaultTheme = {
   .colors = {
      .highlight = GREEN,
      .program_background = (Color){ 235, 235, 255, 255 },
      .bar  = (Color) { 100, 100, 100, 50 },
      .bar_hover = (Color) { 0, 0, 0, 50 },
      .knob = (Color) { 255, 0, 0, 100 },
      .knob_dragging = (Color) { 255, 0, 0, 150 },
      .status_bar_background = GRAY,
   }
};

Theme *theme = &defaultTheme;

//

bool inBar(Vector2 pos) {
   const int margin = 2;
   return pos.x >= program_width - bar_width - margin 
       && pos.x <= program_width + margin
       && pos.y < screenHeight - status_bar_height;
}

enum Type { NONE, INTEGER, FLOAT };

struct Value {
   Type type;
   union {
      int as_int;
      float as_float;
   };

   // WARNING: Ojo con la copia de Value, porque viene de la copia de Stack!
   Value()        : type(NONE)               {}
   Value(int x)   : type(INTEGER), as_int(x) {}
   Value(float f) : type(FLOAT), as_float(f) {}
   // Value(const Value& val) <--- Hay que currarse este bien!

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
   int cursor;
   vector<Stack> timeline;
   static vector<Instr> instrs;
   static InstrFunc start;
   static const char *source;

   Program() : timeline(1), cursor(0) {
      assert(start != 0);
      push("main", start);
   }

   Stack& stack() { return timeline[cursor]; }
   const Stack& stack() const { return timeline[cursor]; }

   int findFunc(InstrFunc fn) {
      for (int i = 0; i < instrs.size(); i++) {
         if (instrs[i].fn == fn) {
            return i;
         }
      }
      return -1;
   }

   void next(InstrFunc fn) { 
      int n = findFunc(fn);
      assert(n != -1);
      stack().top().ip = n;
   }

   bool end()   const { return stack().frames.empty(); }

   int add_local(string name, Value v) {
      Stack::Frame& curr_frame = stack().top();
      assert(curr_frame.names.size() == curr_frame.values.size());
      int idx = curr_frame.names.size();
      curr_frame.names.push_back(name);
      curr_frame.values.push_back(v);
      return idx;
   }

   Value& local(int index) { return stack().top().values[index]; }
   Value& result()         { return stack().result; }

   void push(const char* fname, InstrFunc fn) { 
      int n = findFunc(fn);
      assert(n != -1);
      stack().push(fname, n); 
   }
   void pop() { stack().pop(); }

   void draw_source();
   void draw_stack();
   void draw_status_bar();

   void forwards() {
      assert(cursor >= 0 && cursor < timeline.size());
      if (end()) {
         return;
      }
      cursor++;
      if (cursor >= timeline.size()) {
         timeline.push_back(timeline.back());
         int ip = stack().top().ip;
         (*instrs[ip].fn)(*this, instrs[ip]);
      }
   }

   void backwards() {
      cursor--;
      if (cursor < 0) {
         cursor = 0;
      }
   }

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
      int ip = stack().top().ip;
      Highlight H = instrs[ip].hl;
      if (H.line != -1) {
         pos.x  = 20 + 7.1 * (H.begin - 1) - 1;
         pos.y  = (H.line - 1) * (lineheight) + 4;
         size.x = 7.1 * (H.end - H.begin);
         size.y = lineheight;
         DrawRectangleV(pos, size, theme->colors.highlight);
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

void Program::draw_stack() {
   Vector2 pos;

   // Draw Stack
   pos = { program_width + 20.0f, screenHeight - status_bar_height - 10 };
   Stack& S = stack();
   for (int i = 0; i < S.frames.size(); i++) {
      Stack::Frame& f = S.frames[i];
      Vector2 ini = pos;
      pos.x += 20;
      for (size_t i = 0; i < f.names.size(); i++) {
         pos.y -= 30;
         DrawTextEx(font, f.names[i].c_str(), pos, font.baseSize, 0, BLACK);
         pos.x += 30;
         f.values[i].draw(pos);
         pos.x -= 30;
      }
      pos.y -= 35;
      pos.x -= 10;
      DrawTextEx(font_bold, f.fname, pos, font_bold.baseSize, 0, BLACK);
      pos.x -= 10;
      pos.y -= 25;
      DrawRectangleLines(ini.x - 1, pos.y + 1, 300, ini.y - pos.y - 10, GRAY);
      pos.y -= 10;
   }
}

void Program::draw_status_bar() {
   int top = screenHeight - status_bar_height;
   DrawRectangle(0, top, screenWidth, screenHeight, GRAY);
   int X = status_bar_height;

   Vector2 zero = { 0, 0 };

   // Buttons 
   Color prev_color = BLACK;
   if (CheckCollisionPointRec(GetMousePosition(), (Rectangle) { 0, top, X, X })) {
      prev_color = WHITE;
      if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
         backwards();
      }
   }
   DrawTexturePro(tx_buttons, (Rectangle){   0, 0,  90, 90 }, (Rectangle) { 0, top, X, X }, zero, 0.0f, prev_color);

   Color next_color = BLACK;
   if (CheckCollisionPointRec(GetMousePosition(), (Rectangle) { X, top, X, X })) {
      next_color = WHITE;
      if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
         forwards();
      }
   }
   DrawTexturePro(tx_buttons, (Rectangle){  90, 0,  90, 90 }, (Rectangle) { X, top, X, X }, zero, 0.0f, next_color);

   Color play_color = BLACK;
   if (CheckCollisionPointRec(GetMousePosition(), (Rectangle) { 2*X, top, X, X })) {
      play_color = WHITE;
   }
   DrawTexturePro(tx_buttons, (Rectangle){ 180, 0,  90, 90 }, (Rectangle) { 2*X, top, X, X }, zero, 0.0f, play_color);

   // Timeline bar
   const int nbuttons = 3;
   const int buttons_size = nbuttons * status_bar_height;
   const int bar_size = screenWidth - status_bar_height - buttons_size;


   // Timeline Knob
   static int orig_x;
   static int orig_knob_x;

   const int min_steps = 50;
   int steps = max((int)timeline.size(), min_steps);
   float step_size = float(bar_size)/float(steps);
   const int knob_x = buttons_size + cursor * step_size;
   Color red = theme->colors.knob;
   if (dragging == DRAGGING_STATUS_BAR_KNOB) {
      red = theme->colors.knob_dragging;
   }
   // line
   DrawRectangle(status_bar_height/2 + buttons_size, top + status_bar_height/2, bar_size, 3, (Color){   0, 0, 0, 100 });
   DrawRectangle(status_bar_height/2 + buttons_size, top + status_bar_height/2, (timeline.size() - 1) * step_size, 3, (Color){ 255, 0, 0, 150 });

   DrawTexturePro(tx_buttons, (Rectangle){ 270, 0,  90, 90 }, (Rectangle) { knob_x, top, X, X }, zero, 0.0f, red);

   if (CheckCollisionPointRec(GetMousePosition(), (Rectangle) { knob_x, top, X, X }) &&
       IsMouseButtonPressed(MOUSE_LEFT_BUTTON) &&
       dragging == DRAGGING_NOTHING) 
   {
      dragging = DRAGGING_STATUS_BAR_KNOB;
      orig_x = GetMouseX();
      orig_knob_x = knob_x;
   }
   if (dragging == DRAGGING_STATUS_BAR_KNOB) {
      if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) {
         int dx = GetMouseX() - orig_x;
         int new_cursor = ((orig_knob_x + dx) - buttons_size) / step_size;
         cursor = new_cursor;
         if (new_cursor >= timeline.size()) {
            cursor = timeline.size()-1;
         } else if (new_cursor < 0) {
            cursor = 0;
         } else {
            cursor = new_cursor;
         }
      } else {
         dragging = DRAGGING_NOTHING;
      }
   }
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO: Cargar esto como una DLL... ??
#define DECL(name) void name(Program& P, Instr& I)
#define INSTR(name, code) DECL(name) code

DECL(main_01);
DECL(main_02);
DECL(main_03);
DECL(f_01);
DECL(f_02);
DECL(f_03);

// f(1, 2)
INSTR(main_01, {
   P.next(main_02);
   P.push("f", f_01);
   P.add_local("a", Value(1));
   P.add_local("b", Value(2));
})

// cout << [result] << endl;
INSTR(main_02, {
   int result = P.result().get_int();
   // TODO: Emulate the output!
   cout << result << endl;
   P.pop();
   P.next(main_03);
})

INSTR(main_03, { P.pop(); })

// int c = a + b;
INSTR(f_01, {
   int a = P.local(0).get_int();
   int b = P.local(1).get_int();
   int c = a + b;
   P.add_local("c", Value(c));
   P.next(f_02);
})

// c++;
INSTR(f_02, {
   P.local(2).set_int(P.local(2).get_int() + 1);
   P.next(f_03);
})

// return c;
INSTR(f_03, {
   P.result().set_int(P.local(2).get_int());
   P.pop(); // Esta vuelta está mal, debería ser la siguiente instrucción de la 0 después del 'call'
})

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

InstrFunc Program::start = main_01;
vector<Instr> Program::instrs = {
   { main_01, { 9, 12, 19 } },
   { main_02, { 9, 4, 28 } },
   { main_03, {} },
   { f_01,    { 3, 4, 18 } },
   { f_02,    { 4, 4, 8 } },
   { f_03,    { 5, 4, 13 } },
};

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main() {
   InitWindow(screenWidth, screenHeight, "bola");
   SetConfigFlags(FLAG_VSYNC_HINT);
   SetTargetFPS(60);

   font = LoadSpriteFontEx("iosevka-regular.ttf", 18, 0, 0);
   font_bold = LoadSpriteFontEx("iosevka-bold.ttf", 20, 0, 0);

   tx_buttons = LoadTexture("buttons.png");

   lineheight = font.baseSize + 3;

   int orig_x = -1, orig_program_width;

   Program P;

   RenderTexture2D program_source = LoadRenderTexture(screenWidth, screenHeight - status_bar_height);
   while (!WindowShouldClose())
   {
      // program source
      BeginTextureMode(program_source);
         DrawRectangle(0, 0, screenWidth, screenHeight, theme->colors.program_background);
         P.draw_source();
         Color color_bar = theme->colors.bar;
         if (dragging == DRAGGING_NOTHING && inBar(GetMousePosition())) {
            color_bar = theme->colors.bar_hover;
         }
         DrawRectangle(program_width - bar_width, 0, program_width, screenHeight, color_bar);
      EndTextureMode();

      // whole window
      BeginDrawing();
         DrawTextureRec(program_source.texture, 
                        // WARNING: aquí hace falta poner     -screenHeight,    sino sale al revés
                        (Rectangle){ 0, 0, program_width + 1, -(screenHeight - status_bar_height) },  
                        (Vector2){ 0, 0 }, 
                        WHITE);
         P.draw_stack();
         P.draw_status_bar();
      EndDrawing();

      // Key events
      if (IsKeyPressed(KEY_RIGHT)) {
         P.forwards();
      }
      if (IsKeyPressed(KEY_LEFT)) {
         P.backwards();
      }

      // Drag bar 
      if (inBar(GetMousePosition()) && IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
         dragging = DRAGGING_BAR;
         orig_x = GetMouseX();
         orig_program_width = program_width;
      }
      if (dragging == DRAGGING_BAR) {
         if (IsMouseButtonDown(MOUSE_LEFT_BUTTON)) {
            int new_width = orig_program_width + (GetMouseX() - orig_x); 
            if (new_width >= min_program_width) {
               program_width = new_width;
            }
         } else {
            dragging = DRAGGING_NOTHING;
         }
      }
   }
   CloseWindow();
   return 0;
}
