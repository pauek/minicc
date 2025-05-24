#include <iostream>
#include <vector>
using namespace std;

void escriu_v( const vector<double>& v)
{
  int n= v.size();
  for( int i = 0; i <n ;++i)
  {
    cout << v[i] << endl;
  }
}

void insertion_sort(vector<double>& v, int p)
{
  double x = v[p];
  int i = p-1;
  while( i >= 0 and v[i] > x)
  {
    v[ i +1] = v[i];
    --i;
  }
  v[i+1] = x;
}

void ordena_per_insercio(vector<double>& v)
{
  int n= v.size();
  for( int p =0; p <n; ++p) {
    insertion_sort(v,p);
  }
}

int main()
{
  int n;
  cin >> n;
  vector<double> v(n);
  for( int i =0; i <n; ++i) {
    cin >> v[i];

  }
  ordena_per_insercio(v);
  cout << endl;
  escriu_v(v);

}
