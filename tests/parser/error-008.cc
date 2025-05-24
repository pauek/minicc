#include <vector>
#include <iostream>
using namespace std;

int posicio_maxim(const vector<double>& v, int m){
    int size = v.size() - 1;
    for(int i = size; i >= 0; --i ){
        if (v[i] > v[i - 1]){
            m = i;
        }
    }
    return m;
}