// *******************************************************************************************
// Email: roger.camarena@estudiantat.upc.edu
// Exam: PRO1 - 2025 - Primavera C1_T1_C6S309  Start time: 2025-04-07 15:29:02.561646+02
// Problem: X39980_ca  Submission: E012  Compiler: P1++  Time: 2025-04-07 17:23:33+02
// Verdict: CE 
// Verdict on publics: CE
// Partial scores: Jocs de Prova Públics: 1 - , Correctesa - Només parells: 2 - 
//     Correctesa - Només senars: 2 - , Correctesa - Barreja de parells i senars: 2 - 
//     Eficiència - Seqüències llargues de parells i senars: 3 - 
// *******************************************************************************************



#include <iostream>
using namespace std;

bool parell(int x){
    if(x%2==0) return true;
    else return false;

}

int main(){
int x,y,counterpar=0,countersen=0,maxpar=0,maxsen=0 ;//1 1 
bool trobat=false;
bool trobat2=false;
bool primer=true;
bool primer2=true;
bool mirat=true;
bool mirat1=false;



while(cin>>x ){
    if(mirat){
        y=x;
        mirat=false;
    }
    
    if(mirat1){//saltar primera secuencia; (( 1 y= 1 x=1))
        
        if(parell(x) and parell(y)){
            if(primer){
                counterpar=2;
                primer=false;
            }
            else ++counterpar;
    
            
        }
        else if(!parell(x) and !parell(y)){
            if(primer2){
                countersen=2;
                primer2=false;
            }
            else countersen++;
        }
     
    }
    mirat1=true;
    y=x;//1 1 
    
    if(maxpar<counterpar){// 0<=0 
        
        maxpar=counterpar; //0=0
        if(!parell(x) or !trobat){ //fals
            counterpar=1;
            trobat=true;// // coubnt =0
            primer=true;
        }
    }
    
    if(maxsen<countersen){//0=0 
        
        maxsen=countersen; // 0=0 
        if(parell(x) or !trobat2){ //cert
            countersen=1;
            trobat2=true; //1
            primer2=true;
        }
    
    
    }
   
}   
}
 cout <<"PARELL: "<<maxpar<<endl;
cout<<"SENAR: "<<maxsen<<endl;
[[err]]------------------------------------
tests/parser/error4.cc[87:9-87:9]: No esperaba 'PARELL: ' aquí.
