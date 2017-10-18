
#include <iostream>
#include <vector>
using namespace std;

struct v2 {
   float x, y;
};

v2 average(const vector<v2>& points) {
   vector<v2>::const_iterator it = points.begin();
   v2 sum = {0, 0};
   for (it = points.begin; it != points.end(); it++) {
      sum.x += it->x;
      sum.y += it->y;
   }
   float size = points.size();
   v2 avg = {sum.x / size, sum.y / size};
   return avg;
}

int main() {
   ifstream P("points.txt");
   v2 p;
   vector<v2> points;
   while (P >> p.x >> p.y) {
      points.push_back(p);
   }
   v2 avg = average(points);
   cout << avg.x << ' ' << avg.y << endl;
}



