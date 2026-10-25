#include <iostream>
using namespace std;

int main() {
   string fecha, max_fecha_caja, max_fecha_compra;
   char x;
   double importe, max_compra, max_caja, balance = 0.0;
   string concepto;

   while (cin >> fecha >> x >> importe) {
      getline(cin, concepto);
      concepto = concepto.substr(1);
      if (concepto == "Compra Proveedores") {
         if (max_fecha_compra == "" || importe > max_compra) {
            max_compra = importe;
            max_fecha_compra = fecha;
         }
      }
      if (concepto == "Caja") {
         if (max_fecha_caja == "" || importe > max_caja) {
            max_caja = importe;
            max_fecha_caja = fecha;
         }
      }
      if (x == '-') {
         balance -= importe;
      } else if (x == '+') {
         balance += importe;
      }
   }
   cout << "Balance: " << balance << endl;
   cout << "Compra mayor: " << max_compra << " (" << max_fecha_compra << ")" << endl;
   cout << "Caja mayor: " << max_caja << " (" << max_fecha_caja << ")" << endl;
}
[[in]]--------------------------------------------------
19/03/2014 + 30000.00 Aportación de capital
19/03/2014 - 975.00 Fianza alquiler local
20/03/2014 - 1327.50 Instalación eléctrica
21/03/2014 - 3756.79 Compra Proveedores
23/03/2014 + 256.75 Caja
24/03/2014 + 345.50 Caja
25/03/2014 + 237.50 Caja
25/03/2014 - 546.79 Compra Proveedores
26/03/2014 + 198.80 Caja
27/03/2014 + 396.70 Caja
28/03/2014 + 325.30 Caja
28/03/2014 - 1267.90 Compra Proveedores
29/03/2014 + 216.50 Caja
29/03/2014 - 456.65 Compra Proveedores
[[out]]--------------------------------------------------
Balance: 23646.4
Compra mayor: 3756.79 (21/03/2014)
Caja mayor: 396.7 (27/03/2014)
