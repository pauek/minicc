#ifndef UTIL_HH
#define UTIL_HH

#include <string>

enum Flag {
    Hidden = 1,
    Param = 2,
    Const = 4,
};

template <typename T>
struct Table {
    struct Item {
        std::pair<std::string, T> _value;  // name + value
        unsigned short int        _flags;

        Item(std::string n, T t, int flags = 0) : _value(n, t), _flags(flags) {}

        bool operator==(const Item& i) const {
            return _value == i._value and _flags == i._flags;  // hidden?
        }

        std::string name() const { return _value.first; }

        T value() const { return _value.second; }

        bool has_flag(Flag f) const { return _flags & f; }
    };

    std::vector<Item> tab;
    Item             *_get(std::string name);
    const Item       *_get(std::string name) const;

    int size() const { return tab.size(); }

    std::pair<std::string, T>& operator[](int i) { return tab[i]._value; }

    const std::pair<std::string, T>& operator[](int i) const { return tab[i]._value; }

    void set(std::string name, T value, int flags = 0) {
        Item *i = _get(name);
        if (i == 0) {
            tab.push_back(Item(name, value, flags));
        } else {
            i->_value.second = value;
            i->_flags = flags;
        }
    }

    bool exists(std::string name) const {
        const Item *i = _get(name);
        return i != 0;
    }

    bool get(std::string name, T& res) const {
        const Item *i = _get(name);
        if (i) {
            res = i->_value.second;
        }
        return i != 0;
    }

    bool has_flag(std::string name, Flag f) const {
        const Item *i = _get(name);
        return (i != 0 and i->has_flag(f));
    }

    void set_flag(std::string name, Flag f);
    void remove_flag(std::string name, Flag f);
    void remove_flag_all(Flag f);
    template <class UnaryFunction>
    void for_each(UnaryFunction f);

    bool operator==(const Table& other) const { return tab == other.tab; }
};

template <typename T>
typename Table<T>::Item *Table<T>::_get(std::string name) {
    for (int i = 0; i < tab.size(); i++) {
        if (tab[i]._value.first == name) {
            return &tab[i];
        }
    }
    return 0;
}

template <typename T>
const typename Table<T>::Item *Table<T>::_get(std::string name) const {
    for (int i = 0; i < tab.size(); i++) {
        if (tab[i]._value.first == name) {
            return &tab[i];
        }
    }
    return 0;
}

template <typename T>
void Table<T>::remove_flag_all(Flag f) {
    for (int i = 0; i < tab.size(); i++) {
        tab[i]._flags &= ~f;
    }
}

template <typename T>
void Table<T>::set_flag(std::string name, Flag f) {
    const Item *i = _get(name);
    if (i != 0) {
        i->_flags |= f;
    }
}

template <typename T>
void Table<T>::remove_flag(std::string name, Flag f) {
    const Item *i = _get(name);
    if (i != 0) {
        i->_flags &= ~f;
    }
}

template <typename T>
template <class UnaryFunction>
void Table<T>::for_each(UnaryFunction f) {
    for (int i = 0; i < tab.size(); i++) {
        f(tab[i]._value.second);
    }
}

#endif
