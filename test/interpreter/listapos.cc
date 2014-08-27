#include <iostream>
using namespace std;

struct Pos {
   int x, y;
};

struct TablaPos {
   Pos pos[10];
   int npos;
};

int main() {
   TablaPos T;
   T.npos = 1;
   T.pos[0].x = 5;
   T.pos[1].y = 3;
   cout << T.pos[0].x << ' ' << T.pos[1].y << endl;   
}
[[out]]--------------------------------------------------
5 3
[[err]]--------------------------------------------------
