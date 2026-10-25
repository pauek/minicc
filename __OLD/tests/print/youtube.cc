
#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <map>
using namespace std;

struct  Comentario  {
   string fecha,   usuario,  texto;
};

int main() {
   map<int,list<Comentario> > M;
   vector<int> Grupo; // a qué bloque pertenece un comentario
   Comentario C;
   int total, n, r;
   string codigo;
   cin >> codigo;
   codigo += ".txt";
   ifstream F(codigo.c_str());
   F >> total;
   Grupo.resize(total+1, -1);
   while (F >> n >> r >> C.fecha >> C.usuario) {
      getline(F, C.texto);
      if (r == -1) {
         M[n].push_back(C);
         Grupo[n] = n;
      } else {
         int g = Grupo[r];
         M[g].push_back(C);
         Grupo[n] = g;
      }
   }
   list<pair<int, int> > L;
   map<int, list<Comentario> >::iterator it;
   for (it = M.begin(); it != M.end(); it++) {
      L.push_back(make_pair(it->second.size(), it->first));
   }
   L.sort();
   list< pair<int, int> >::reverse_iterator rit;
   for (rit = L.rbegin(); rit != L.rend(); rit++) {
      list<Comentario>& L = M[rit->second];
      list<Comentario>::iterator jt = L.begin();
      cout << jt->fecha << " (" << jt->usuario << "): " << jt->texto << endl;
      for (jt++; jt != L.end(); jt++) {
         cout << "   " << jt->fecha << " (" << jt->usuario << "): " << jt->texto << endl;
      }
   }
}
[[out]]-------------------------------------------------

#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <map>
using namespace std;

struct Comentario {
   string fecha, usuario, texto;
};

int main() {
   map<int, list<Comentario>> M;
   vector<int> Grupo; // a qué bloque pertenece un comentario
   Comentario C;
   int total, n, r;
   string codigo;
   cin >> codigo;
   codigo += ".txt";
   ifstream F(codigo.c_str());
   F >> total;
   Grupo.resize(total + 1, -1);
   while (F >> n >> r >> C.fecha >> C.usuario) {
      getline(F, C.texto);
      if (r == -1) {
         M[n].push_back(C);
         Grupo[n] = n;
      } else {
         int g = Grupo[r];
         M[g].push_back(C);
         Grupo[n] = g;
      }
   }
   list<pair<int, int>> L;
   map<int, list<Comentario>>::iterator it;
   for (it = M.begin(); it != M.end(); it++) {
      L.push_back(make_pair(it->second.size(), it->first));
   }
   L.sort();
   list<pair<int, int>>::reverse_iterator rit;
   for (rit = L.rbegin(); rit != L.rend(); rit++) {
      list<Comentario>& L = M[rit->second];
      list<Comentario>::iterator jt = L.begin();
      cout << jt->fecha << " (" << jt->usuario << "): " << jt->texto << endl;
      for (jt++; jt != L.end(); jt++) {
         cout << "   " << jt->fecha << " (" << jt->usuario << "): " << jt->texto << endl;
      }
   }
}
