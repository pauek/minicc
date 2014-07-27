#include <iostream>
using namespace std;

bool equal_ends(string a, string b){
    int a0, b0, af, bf;
    a0=int(a[0]);
    b0=int(b[0]);
    af = a.size()-1;
    bf = b.size()-1;
    if (a.empty() or b.empty()) return (false);
    else if ((a[0] == b[0] && (af == bf))) return (true);
    else if (a0==b0+32 && af==bf+32) return (true);
    else if (a0+32==b0 && af+32==bf) return (true);
    else if (a0+32==b0 && af==bf) return (true);
    else if (a0==b0 && af+32==bf) return (true);
    else if (a0==b0+32 && af==bf) return (true); 
    else if (a0+32==a0 && af==bf+32)return(true);
    else return (false);
}

int main (){
   string a, b;
   getline (cin, a);
   getline (cin, b);
   if (equal_ends(a, b))cout << "Si" << endl;
   else cout << "No" << endl;
   return 0;                  
}
[[out]]--------------------------------------------------
#include <iostream>
using namespace std;

bool equal_ends(string a, string b) {
   int a0, b0, af, bf;
   a0 = int(a[0]);
   b0 = int(b[0]);
   af = a.size() - 1;
   bf = b.size() - 1;
   if (a.empty() or b.empty()) return (false);
   else if ((a[0] == b[0] && (af == bf))) return (true);
   else if (a0 == b0 + 32 && af == bf + 32) return (true);
   else if (a0 + 32 == b0 && af + 32 == bf) return (true);
   else if (a0 + 32 == b0 && af == bf) return (true);
   else if (a0 == b0 && af + 32 == bf) return (true);
   else if (a0 == b0 + 32 && af == bf) return (true);
   else if (a0 + 32 == a0 && af == bf + 32) return (true);
   else return (false);
}

int main() {
   string a, b;
   getline(cin, a);
   getline(cin, b);
   if (equal_ends(a, b)) cout << "Si" << endl;
   else cout << "No" << endl;
   return 0;
}
[[err]]--------------------------------------------------
