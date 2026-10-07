#include <iostream>
#include <vector>
using namespace std;

struct Int {
    int value;
};

struct Double {
    double value;
};

class AST {
   public:
    template <typename T>
    struct Ref {
        size_t k;

        T& get() { return Class<T>::instances_[k]; }
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

    void test() {
        auto r1 = Class<Int>::create({5});
        auto r2 = Class<Double>::create({0.01});
        cout << r1.get().value << endl;
        cout << r2.get().value << endl;
    }
};

template<>
vector<Int>    AST::Class<Int>::instances_ = {};

template<>
vector<Double> AST::Class<Double>::instances_ = {};

int main() {
    AST ast;
    ast.test();
}
