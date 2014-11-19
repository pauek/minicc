using namespace std;

struct X {
   int a, b;
};

int main() {
   int a, b, c;
   X T[10];
   a = 1;
   b = 2;
   c = 3;
   for (int i = 0; i < 10; i++) {
   	T[i].a = i % 2;
      T[i].b = i;
   }
   cout << T[0].a << endl;
   cout << T[1].b << endl;
}
[[out]]--------------------------------------------------
0
1
