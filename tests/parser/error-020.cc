#include <iostream>
#include <string>
#include <algorithm>
#include <vector>
using namespace std;


struct Ma {
    int ma;
    int g1;
    char l1;
    int g2;
    char l2;
    
};
    

bool ordena_ma(const char& a, const char& b) {
    return (a < b);
    
}


bool ordena_ma_n(const int& a, const int& b) {
    return (a > b);
    
}


vector<char> convert(string& m) {
    vector<char> v(m.length(), '0');
    
    for (int i = 0; i < m.length(); ++i) {
	if (m[i] == 'R') v[i] = 'B';
	else if (m[i] == 'Q') v[i] = 'C';
	else if (m[i] == 'C') v[i] = 'D';
	else if (m[i] == 'D') v[i] = 'E';
	else if (m[i] == 'N') v[i] = 'F';
	else v[i] = m[i];
	
    }
    
    return v;
    
}


vector<int> convert_n(string& m) {
    vector<int> v(m.length(), 1);
    
    for (int i = 0; i < m.length(); ++i) {
	if (m[i] == 'A') v[i] = 6;
	else if (m[i] == 'R') v[i] = 5;
	else if (m[i] == 'Q') v[i] = 4;
	else if (m[i] == 'C') v[i] = 3;
	else if (m[i] == 'D') v[i] = 2;
	else if (m[i] == 'N') v[i] = 1;
	
    }
    
    return v;
    
}


int sumador_ma(const vector<int>& n) {
    int pre;
    int suma = 0;
    int cont = 1;
    
    int i = 0;
    while(i < n.size()) {
	if (i == 0) pre = n[i];
	else {
	    if (n[i] != pre) suma += n[i];
	    else {
		cont *= 100;
		suma += n[i] * cont;
	    }
	    
	}
	++i;
    }  
       
    return suma;
    
}


Ma valor_ma(const vector<char>& v) {
    char pre;
    int cont = 0;
    int parell = 0;
    int trio = 0;
    int poker = 0;
    int repoker = 0;
    int i = 0;
    Ma ma;
    ma.g1 = 0;
    ma.l1 = '0';
    ma.g2 = 0;
    ma.l2 = '0';
    while(i < v.size()) {
	if (i == 0) pre = v[i];
	else {
	    if (v[i] == pre) ++cont;
	    else if (cont > 0) {
		if (cont == 1) {
		    ++parell;
		    if (ma.l1 == '0') {
			ma.l1 = v[i - 1];
			ma.g1 = 1;
			
		    } else {
			ma.l2 = v[i - 1];
			ma.g1 = 1;
			
		    }
		    
		}
		if (cont == 2) {
		    ++trio;
		    if (ma.l1 == '0') {
			ma.l1 = v[i - 1];
			ma.g1 = 2;
			
		    } else {
			ma.l2 = v[i - 1];
			ma.g1 = 2;
			
		    }
		    
		}
		if (cont == 3) ++poker;
		if (cont == 4) ++repoker;
		cont = 0;
		pre = v[i];
		
	    }
	    
	}
	++i;
    }
    
    if (cont > 0) {
	if (cont == 1) {
	    ++parell;
	    if (ma.l1 == '0') {
		ma.l1 = v[i - 1];
		ma.g1 = 1;
		
	    } else {
		ma.l2 = v[i - 1];
		ma.g1 = 1;
		
	    }
	    
	}
	
	if (cont == 2) {
	    ++trio;
	    if (ma.l1 == '0') {
		ma.l1 = v[i - 1];
		ma.g1 = 2;
		
	    } else {
		ma.l2 = v[i - 1];
		ma.g1 = 2;
		
	    }
	    
	}
	if (cont == 3) ++poker;
	if (cont == 4) ++repoker;
	cont = 0;
	pre = v[i];
	
    }
        
    if (parell == 1 and trio == 0) ma.ma = 1;
    else if (parell == 2) ma.ma = 2;
    else if (trio == 1 and parell == 0) ma.ma = 3;
    else if (parell == 1 and trio == 1) ma.ma = 4;
    else if (poker == 1) ma.ma = 5;
    else if (repoker == 1) ma.ma = 6;
    else ma.ma = 0;
    
    if (ma.g1 == ma.g2) {
	if (ma.l1 > ma.l2) {
	    char x = ma.l1;
	    ma.l1 = ma.l2;
	    ma.l2 = x;
	}
    } else {
	if (ma.g1 < ma.g2) {
	    int y = ma.g1;
	    ma.g1 = ma.g2;
	    ma.g2 = y;
	    char x = ma.l1;
	    ma.l1 = ma.l2;
	    ma.l2 = x;
	}
    }
    
    return ma;
	    
    
}


bool guanyador(const Ma& ma1, const Ma& ma2, vector<char>& m1, vector<char>& m2) {
    if (ma1.ma > ma2.ma) return true;
    if (ma1.ma < ma2.ma) return false;
    if (ma1.l1 < ma2.l1) return true;
    if (ma1.l1 > ma2.l1) return false;
    if (ma1.l2 < ma2.l2) return true;
    if (ma1.l2 > ma2.l2) return false;
    
    int i = 0;
    while(i < m1.size()) {
	if (m1[i] < m2[i]) return true;
	if (m1[i] > m2[i]) return false;
	++i;
    }
    
    if (i == m1.size()) return true;
    return false;
    
}


int main() {
    string s1, s2;
    int anna = 0;
    int bernat = 0;
    while(cin >> s1 >> s2) {
	//vector<int> n1 = convert_n(s1);
	//vector<int> n2 = convert_n(s2);
	
	vector<char> m1 = convert(s1);
	vector<char> m2 = convert(s2);
	
	sort(m1.begin(), m1.end(), ordena_ma);
	sort(m2.begin(), m2.end(), ordena_ma);
	
	//sort(n1.begin(), n1.end(), ordena_ma_n);
	//sort(n2.begin(), n2.end(), ordena_ma_n);
	
	//int na1 = sumador_ma(n1);
	//int na2 = sumador_ma(n2);
	
	Ma ma1 = valor_ma(m1);
	Ma ma2 = valor_ma(m2);
	
	//for(int i = 0; i < m1.size(); ++i) cout << m1[i];
	//cout << endl;
	//for(int i = 0; i < m1.size(); ++i) cout << m2[i];
	//cout << endl;
	//cout << na1 << ":" << na2 << endl;
	
	bool win1 = false;	
	win1 = guanyador(ma1, ma2, m1, m2);
	
	if (win1) {
	    cout << "Anna" << endl;
	    ++anna;
	} else {
	    cout << "Bernat" << endl;
	    ++bernat;
	}    
	
    }
    cout << "Guanyades per l'Anna: " << anna << endl;
    cout << "Guanyades per en Bernat: " << bernat << endl;
    
}