void ordena_per_insercio(vector<double>& v)
{
    for (int i = 1; i < v.size(); ++i) {
        double aux = v[i];
        int l = i;
        while (l > 0 and v[l - 1] > aux;) {
            v[l] = v[l - 1];
            --l;
        }
        v[l] = aux;
    }
}
[[err]]------------------------------------------------------
tests/parser/error12.cc[6:40]: Esperaba ')' aquí.
