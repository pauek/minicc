#ifndef UTIL_HH
#define UTIL_HH

template<typename T>
class SimpleTable {
protected:
   struct Item {
      std::pair<std::string, T> _data; // name + data
      bool                      _hidden;

      Item(std::string n, T d, bool h = false) : _data(n, d), _hidden(h) {}

      bool operator==(const Item& i) const {
         return _data == i._data and _hidden == i._hidden; // hidden?
      }

      std::string name() const { return _data.first; }
      T data() const { return _data.second; }
   };

   std::vector<Item> tab;
   Item *_get(std::string name);
   
public:
   int size() const { return tab.size(); }
   const std::pair<std::string, T>& operator[](int i) const { 
      return tab[i]._data; 
   }

   void set(std::string name, T data, bool hidden = false) {
      Item *i = _get(name);
      if (i == 0) {
         tab.push_back(Item(name, data, hidden));
      } else {
         i->_data.second = data;
      }
   }

   bool get(std::string name, T& res) {
      Item *i = _get(name);
      if (i) {
         res = i->_data.second;
      }
      return i != 0;
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

#endif
