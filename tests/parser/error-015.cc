#include <iostream>
using namespace std;

//Pre donada una n >= 0;¡
//Post  si es bicolor retorna cert, si no fals

bool is_bicolor(int n) {
    int digito1 = n;
    int suma = -1;
    int digito2 = -1;
    //El while dona els dos ultims digits del numero i mira si son iguals.
    while (n > 0) { 
        digito1 = n % 10; //Calcula l'ultim digit
        n = n / 10; 

        if (digito1 != digito2) {
            suma += 1;
        }

        //Si la suma de digits diferents es mes de 1 retorna fals
        if (suma > 1) {
            return false;
        }

        digito2 = digito1;
    }

    //La suma de digits diferents es menys de 2;
    else {
        return true;
    }
}

int main () {
    int n;
    cin >> n;
    if (is_bicolor(n) == true) {
        cout << "true" << endl;
    }

    else {
        cout << "false" << endl;
    }
}
[[err]]---------------------------------
tests/parser/error-015.cc[29:5]: No esperaba 'else' aquí.
