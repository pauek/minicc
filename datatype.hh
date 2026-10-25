#ifndef AST2_HH
#define AST2_HH

#include <cassert>
#include <cstdint>
#include <type_traits>
#include <vector>
using namespace std;

// A TypeID identifies each type and also particular values
typedef uint32_t TypeID;

// Constants
constexpr TypeID None = uint32_t(-1);
constexpr size_t NoIndex = size_t(-1);

// Concept to check templates for a struct
template <class T>
concept IsStruct = std::is_class_v<T>;

// A Store keeps all instances of a certain type
template <IsStruct T>
struct Store {
    static vector<T> instances;
};

template <IsStruct T>
vector<T> Store<T>::instances = {};

// A Ref is a "pointer" to a struct. It is the index into de Type<T>::instances_ vector.
template <IsStruct T>
struct Ref {
    // index into Store<T>::instances
    size_t index = NoIndex;  // initialize to NoIndex

    // Allow a reference to be empty
    Ref() : index(NoIndex) {}
    bool empty() const { return index == NoIndex; }

    // Normal constructor for more idiomatic initialization
    Ref(size_t index_) : index(index_) {}

    T& get() {
        assert(index != NoIndex);
        return Store<T>::instances[index];
    }

    const T& get() const {
        assert(index != NoIndex);
        return Store<T>::instances[index];
    }

    uint32_t type_id() const { return T::type_id; }

};

template <IsStruct T, TypeID ID>
struct DataType {
    static constexpr TypeID type_id = ID;

    static Ref<T> make(const T& t) {
        Store<T>::instances.push_back(t);
        return Ref<T>(Store<T>::instances.size() - 1);
    }
};

// A variant is like a Ref<T> but with a type that
// can be in the interval [First, Last], so it
// can be one of several things

template <TypeID First, TypeID Last>
struct Variant {
    TypeID type_id = None;   // TypeID of the active type
    size_t index = NoIndex;  // Index into the Store<Type>::instances

    explicit Variant() : type_id(None), index(NoIndex) {}

    template <IsStruct T>
    explicit Variant(const Ref<T>& ref) {
        // Ensure TypeID of T is between limits
        static_assert(T::type_id >= First && T::type_id <= Last);
        type_id = T::type_id;
        index = ref.index;
    }

    template <IsStruct T>
    T& as() {
        assert(T::type_id == type_id && index != NoIndex);
        return Ref<T>{index}.get();
    }

    template <IsStruct T>
    const T& as() const {
        assert(T::type_id == type_id && index != NoIndex);
        return Ref<T>(index).get();
    }

    template <IsStruct T>
    bool is() {
        return type_id == T::type_id;
    }
};

#endif
