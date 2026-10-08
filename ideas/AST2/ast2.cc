#include <iostream>
using namespace std;

#include "ast2.hh"

struct Int {
    int value;
};

struct Double {
    double value;
};

typedef Variant2<Int, Double> Number;

///

int main() {
    auto r1 = Type<Int>::create({5});
    cout << r1.get().value << endl;

    auto r2 = Type<Double>::create({0.01});
    cout << r2.get().value << endl;

    auto r3 = Type<Number>::create({ r1 });
    cout << r3.get().as<Int>().value << endl;
    cout << r3.get().is<Double>() << endl;
}
