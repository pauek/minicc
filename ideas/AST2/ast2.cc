#include <iostream>
#include <vector>
using namespace std;

template <typename T>
struct Ref {
    size_t index;

    Ref(size_t i) : index(i) {}

    T& get() { return T::instances_[index]; }
};

template <typename T>
class DataDriven {
    friend class Ref<T>;

    static vector<T> instances_;

   protected:
    static Ref<T> new_instance(const T& t) {
        size_t id = instances_.size();
        instances_.push_back(t);
        return Ref<T>(id);
    }
};

template <typename T>
vector<T> DataDriven<T>::instances_ = {};

struct IntLiteral : DataDriven<IntLiteral> {
    int value;

    static Ref<IntLiteral> create(int n) { return new_instance({.value = n}); }
};

struct DoubleLiteral : DataDriven<DoubleLiteral> {
    double value;

    static Ref<DoubleLiteral> create(double x) { return new_instance({.value = x}); }
};

int main() {
    auto r1 = IntLiteral::create(5);
    auto r2 = IntLiteral::create(123);
    cout << r1.get().value << endl;
    cout << r2.get().value << endl;

    auto r3 = DoubleLiteral::create(0.001);
    cout << r3.get().value << endl;
}
