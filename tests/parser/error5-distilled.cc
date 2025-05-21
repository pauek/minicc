// *******************************************************************************************
// Email: clara.garcia.gonzalez@estudiantat.upc.edu
// Exam: PRO1 - 2025 - Primavera C1_T1_C6S302  Start time: 2025-04-07 15:32:22.811531+02
// Problem: X39980_ca  Submission: E002  Compiler: P1++  Time: 2025-04-07 16:05:44+02
// Verdict: SC 
// Verdict on publics: AC, AC, AC
// Score: 6/10
// Partial scores: Correctesa - Només parells: 0 - WA, Correctesa - Només senars: 0 - WA
//     Correctesa - Barreja de parells i senars: 2 - AC
//     Eficiència - Seqüències llargues de parells i senars: 4 - AC
// *******************************************************************************************



//subsequencia
#include <iostream>

using namespace std;

bool es_parell(int n){
    if (n % 2 == 0) return true;
    return false;
}

int main(){
    int n, n_parells_max = 0, n_senars_max = 0, senars_aux = 1, parells_aux = 1;
    bool parell;
    cin >> n;
    parell = es_parell(n);
    while(cin >> n) {
        if (parell) { // l'anterior era parell
            if (es_parell(n)) parells_aux ++;
            else {
                if (parells_aux > n_parells_max) n_parells_max = parells_aux;
                parells_aux = 1;
                parell = false;
            }
        }
        else {
            if (!es_parell(n)) senars_aux ++;
            else {
                if (senars_aux > n_senars_max) n_senars_max = senars_aux;
                senars_aux = 1;
                parell = true;
            }
        }
    }
    if (senars_aux > n_senars_max) n_senars_max = senars_aux;
    if (parells_aux > n_parells_max) n_parells_max = parells_aux;
    cout << "PARELL: "<<n_parells_max << endl<< "SENAR: " << n_senars_max << endl;
}
