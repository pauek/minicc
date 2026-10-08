#ifndef AST2_HH
#define AST2_HH

#include <cassert>
#include <variant>
#include <vector>
using namespace std;

template <typename T>
struct Ref {
    size_t k;
    T&     get();
};

template <typename T>
struct Type {
    static vector<T> instances_;

    static Ref<T> create(const T& t) {
        size_t k = instances_.size();
        instances_.push_back(t);
        return {k};
    }
};

template <typename T>
vector<T> Type<T>::instances_ = {};

template <typename T>
T& Ref<T>::get() {
    return Type<T>::instances_[k];
}

template <size_t, typename... RefTypes>
struct Variant {
    variant<RefTypes...> value;

    template <typename T>
    bool is() {
        return holds_alternative<Ref<T>>(value);
    }

    template <typename T>
    T& as() {
        assert(holds_alternative<Ref<T>>(value));
        return get<Ref<T>>(value).get();
    }
};

template <typename A, typename B>
struct Variant2 : Variant<2, Ref<A>, Ref<B>> {};

template <typename A, typename B, typename C>
struct Variant3 : Variant<2, Ref<A>, Ref<B>, Ref<C>> {};

template <typename A, typename B, typename C, typename D>
struct Variant4 : Variant<2, Ref<A>, Ref<B>, Ref<C>, Ref<D>> {};

template <typename A, typename B, typename C, typename D, typename E>
struct Variant5 : Variant<2, Ref<A>, Ref<B>, Ref<C>, Ref<D>, Ref<E>> {};

template <typename A, typename B, typename C, typename D, typename E, typename F>
struct Variant6 : Variant<2, Ref<A>, Ref<B>, Ref<C>, Ref<D>, Ref<E>, Ref<F>> {};

#endif
