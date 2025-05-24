int posicio (double x, const vector<double>& v, int esq, int dre ) {
  auto it = lower_bound(v.begin() + esq, v.begin() + dre + 1, x);
  int pos = it - v.begin();
  if (pos <= dre) return pos;
  else return -1;
}