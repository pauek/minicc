#ifndef UTIL_HH
#define UTIL_HH

enum Flag { Hidden = 1, Param = 2 };

template<typename T>
struct SimpleTable {
   struct Item {
      std::pair<std::string, T> _data; // name + data
      unsigned short int        _flags;

      Item(std::string n, T d, int flags = 0) : _data(n, d), _flags(flags) {}

      bool operator==(const Item& i) const {
         return _data == i._data and _flags == i._flags; // hidden?
      }

      std::string name()     const { return _data.first; }
                T data()     const { return _data.second; }
             bool has_flag(Flag f) const { return _flags & f; }
   };

   std::vector<Item> tab;
   Item *_get(std::string name);
   const Item *_get(std::string name) const;
   
   int size() const { return tab.size(); }
   const std::pair<std::string, T>& operator[](int i) const { 
      return tab[i]._data; 
   }

   void set(std::string name, T data, int flags = 0) {
      Item *i = _get(name);
      if (i == 0) {
         tab.push_back(Item(name, data, flags));
      } else {
         i->_data.second = data;
         i->_flags = flags;
      }
   }

   bool exists(std::string name) const {
      const Item *i = _get(name);
      return i != 0;
   }

   bool get(std::string name, T& res) {
      Item *i = _get(name);
      if (i) {
         res = i->_data.second;
      }
      return i != 0;
   }

   bool has_flag(std::string name, Flag f) const {
      const Item *i = _get(name);
      return (i != 0 and i->has_flag(f));
   }

   bool operator==(const SimpleTable& other) const {
      return tab == other.tab;
   }
};

template<typename T>
typename SimpleTable<T>::Item *SimpleTable<T>::_get(std::string name) {
   for (int i = 0; i < tab.size(); i++) {
      if (tab[i]._data.first == name) {
         return &tab[i];
      }
   }
   return 0;
}

template<typename T>
const typename SimpleTable<T>::Item *SimpleTable<T>::_get(std::string name) const {
   for (int i = 0; i < tab.size(); i++) {
      if (tab[i]._data.first == name) {
         return &tab[i];
      }
   }
   return 0;
}

#endif
