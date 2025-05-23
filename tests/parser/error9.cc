#include <iostream>
#include <vector>

using namespace std; 

void ordena_per_insercio(vector <double>& v){
	unsigned int s = v.size(), pos, aux; 
	for(unsigned int i = 0; i < s; i++){
		pos = i; 
		aux = v[i]; 
		
		while(pos > 0 and v[pos - 1] > aux){
			v[pos] = v [pos -1];
			pos --;
		}
		v[pos] = aux; 
	}
}
