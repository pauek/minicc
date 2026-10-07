#include <iostream>
#include <vector>
using namespace std;

struct Int {
    int value;
};

struct Double {
    double value;
};

template <typename T>
struct Ref {
    size_t k;
    T& get();
};

template <typename T>
struct Class {
    static vector<T> instances_;

    static Ref<T> create(const T& t) {
        size_t k = instances_.size();
        instances_.push_back(t);
        return {k};
    }
};

template <typename T>
T& Ref<T>::get() {
    return Class<T>::instances_[k];
}

template <>
vector<Int> Class<Int>::instances_ = {};

template <>
vector<Double> Class<Double>::instances_ = {};

int main() {
    auto r1 = Class<Int>::create({5});
    cout << r1.get().value << endl;

    auto r2 = Class<Double>::create({0.01});
    cout << r2.get().value << endl;
}
