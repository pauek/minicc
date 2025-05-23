#include <iostream>
#include <vector>
using namespace std;

void swap(int& a, int& b)
{
    int aux = b;
    b = a;
    a = aux;
}

void insereix(vector<double>& v, i)
{
    int i = i - 1;
    while (v[i] < v[i - 1] and i > 0) {
        swap(v[i], v[i - 1]);
        i--;
    }
}

void ordena_per_insercio(vector<double>& v)
{
    int n = v.size();
    for (int i = 0; i < n; ++i) {
        insereix(v, i);
        ++i;
    }
}
