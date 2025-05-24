#include <iostream>
#include <vector>
using namespace std;

vector<int> calcula_cims(const vector<int>& v){
    vector<int> cims(v.size() , 0) ;
    for ( int i = 1 ; i < v.size() - 1 ; i ++ ) {
        if ( v[i] > v[i - 1] and v[i] > v[i + 1] ) cims[i] = v[i] ;
    }
    return cims ;
}