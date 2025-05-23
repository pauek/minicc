// *******************************************************************************************
// Email: alejandro.romero.diaz@estudiantat.upc.edu
// Exam: PRO1 - 2025 - Primavera C1_T1_C6S303  Start time: 2025-04-07 15:31:36.322447+02
// Problem: X39980_ca  Submission: E001  Compiler: P1++  Time: 2025-04-07 15:43:06+02
// Verdict: CE 
// Verdict on publics: CE
// Partial scores: Correctesa - Només parells: 2 - , Correctesa - Només senars: 2 - 
//     Correctesa - Barreja de parells i senars: 2 - 
//     Eficiència - Seqüències llargues de parells i senars: 4 - 
// *******************************************************************************************



#include <iostream>;
using namespace std;

int main(){
    int x1, count;
    count = 0;
    bool firstNum = true;
    bool parell = false;
    while (cin >> x){
        if(x % 2 == 0){
            if(parell){
                count++;
            }else{
                if(count > maxParellCount){
                    maxParellCount = count;
                }
                count = 1;
            }
        }else{ //Si el nombre que entra es senar
            if(!parell){
                count++;
            }else{
                if(count > maxSenarCount){
                    maxSenarCount = count;
                }
                count = 1;
            }
        }
    }
    cout << "PARELL: " << maxParellCount << endl;
    cout << "SENAR: " << maxSenarCount;
}
[[err]]-----------------------------------------------
tests/parser/error3.cc[14:20-14:20]: No termines los #includes con punto y coma.
