#include <iostream>
#include <vector>
using namespace std;

struct Gasto {
   double fuera, total; // dentro y fuera de la zona de cada tienda.
};

typedef map<string, map<int, bool> > Tiendas;

bool dentro(Tiendas& T, string tienda, int cp) {
   Tiendas::iterator it = T.find(tienda);
   return it != T.end() && it->second.find(cp) != it->second.end();
}
[[out]]--------------------------------------------------
#include <iostream>
#include <vector>
using namespace std;

struct Gasto {
   double fuera, total; // dentro y fuera de la zona de cada tienda.
};

typedef map<string, map<int, bool>> Tiendas;

bool dentro(Tiendas& T, string tienda, int cp) {
   Tiendas::iterator it = T.find(tienda);
   return it != T.end() && it->second.find(cp) != it->second.end();
}
