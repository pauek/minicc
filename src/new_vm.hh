
#ifndef VM_HH
#define VM_HH

#include <string>
#include <vector>
#include <map>
#include "new_types.hh"

namespace vm {

typedef size_t mem_index_t;

// NameTable

class NameTable {
   std::vector<std::string> _names;
public:
   size_t Put(std::string name) {
      for (size_t i = 0; i < _names.size(); i++) {
         if (name == _names[i]) return i;
      }
      size_t index = _names.size();
      _names.push_back(name);
      return index;
   }

   std::string Get(size_t index) {
      assert(index < _names.size());
      return _names[index];
   }
};

const size_t MiB = 1024 * 1024;

class Memory {
   struct Chunk {
      Type type;
      size_t start, size;
      Chunk(Type _type, size_t _start, size_t _size) 
         : type(_type), start(_start), size(_size) {}
   };

   TypeTable& _type_table;
   void *data;
   size_t total_size;
   /* 
      We want the indices of memory chunks to keep growing. They are like IDs.
      If you free a chunk and by mistake you still have a pointer to this chunk 
      somewhere, and then you allocate, by recycling indices you can make the 
      old pointer (which was pointing to old memory) to "be right" again. 
      We can avoid this by making new indices all the time, so that the index 
      is tied to the identity of the object that was allocated at the time.
      When there is a pointer to a freed chunk, it can never work, because the
      index will have disappeared, so we can detect this pointers.

   */
   mem_index_t last = 0; // Indices keep growing
   std::map<mem_index_t, Chunk> _chunks;
   std::map<size_t, mem_index_t> _map;

public:
   Memory(size_t size, TypeTable& type_table);

   bool Alloc(Type t, mem_index_t& index);
   void Free(mem_index_t);

   const Chunk *Get(mem_index_t index)  const;
};

// VM

class VM {
   NameTable  _names;
   TypeTable  _types;
   Memory     _memory;
};

}

#endif