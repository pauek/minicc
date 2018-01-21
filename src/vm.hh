
#ifndef VM_HH
#define VM_HH

#include <string>
#include <vector>
#include <map>
#include <stack>

namespace vm {

typedef uint16_t type_index_t;
typedef size_t   name_index_t;
typedef size_t   mem_index_t;
typedef size_t   func_index_t;

// Types ///////////////////////////////////////////////////////////////////////

enum TypeTag      : uint8_t { I32, U32, F32, I64, U64, F64, Composite };
enum TypeDescrTag : uint8_t { Struct, Array };

struct Type {  // This has a constant size (4 bytes), which is important
   TypeTag      tag;
   type_index_t index; // index in the type description table

   // TODO: Maybe use more space for the index? There is one spare byte...

   Type(TypeTag _tag, type_index_t _index = 0) {
      tag   = _tag;
      index = _index;
   }
};

struct TypeDescr { // The layout description of a compound type
   name_index_t name;    // index to the name table
   TypeDescrTag Tag() const { return _tag; }
protected:
   TypeDescrTag _tag;
};

template<TypeDescrTag T>
struct TypeDescrDerived : TypeDescr {
   static bool is_instance(const TypeDescr *desc) { return desc->Tag() == T; }
   TypeDescrDerived() { _tag = T; }
};

struct Field {
   name_index_t name;
   Type    type;
};

struct StructDescr : TypeDescrDerived<Struct> {
   std::vector<Field> fields;
   StructDescr(name_index_t _name, const std::vector<Field>& _fields) {
      name   = _name;
      fields = _fields;
   }
};
struct ArrayDescr : TypeDescrDerived<Array> {
   size_t size;
   Type   elem_type;
   ArrayDescr(name_index_t _name, size_t _size, Type _elem_type) 
      : size(_size), elem_type(_elem_type) { name = _name; }
};

// TypeTable ///////////////////////////////////////////////////////////////////

class TypeTable {
   std::vector<TypeDescr*> _types;

public:
   TypeDescr *Get(type_index_t index) {
      assert(index >= 0 && index < _types.size());
      return _types[(size_t)index];
   }

   size_t SizeOf(Type type);
   type_index_t Add(TypeDescr *desc);
};

// NameTable ///////////////////////////////////////////////////////////////////

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

// Memory //////////////////////////////////////////////////////////////////////

const size_t MiB = 1024 * 1024;

class Memory {
   struct Chunk {
      Type type;
      size_t start, size;

      Chunk(Type _type, size_t _start, size_t _size) 
         : type(_type), start(_start), size(_size) {}
   };

   TypeTable& _type_table;
   uint8_t *data;
   size_t   total_size, heap_size, stack_pos, stack_top;
   /* 
      We want the indices of memory chunks to keep growing. They are like IDs.
      If you free a chunk and by mistake you still have a pointer to this chunk 
      somewhere, and then you allocate, by recycling indices you can make the 
      old pointer (which was pointing to old memory) to "be right" again. 
      We can avoid this by making new indices all the time, so that the index 
      is tied to the identity of the object that was allocated at the time.
      When there is a pointer to a freed chunk, it can never work, because the
      index will have disappeared, therefore we can detect this pointers.

   */
   mem_index_t last = 0; // Indices keep growing
   std::map<mem_index_t, Chunk> _chunks;
   std::map<size_t, mem_index_t> _map;
   std::stack<mem_index_t> _stack; // stack of indices to chunks

   mem_index_t NewIndex() {
      mem_index_t index = last + 1;
      last = index;
      return index;
   }

public:
   Memory(size_t sz_heap, size_t sz_stack, TypeTable& type_table);
   bool Alloc(Type t, mem_index_t& index);
   void Free(mem_index_t);
   bool StackPush(Type t, mem_index_t& index);
   mem_index_t StackTop() const { return _stack.top(); }
   void StackPop();
   const Chunk *Get(mem_index_t index)  const;

   template<typename T>
   bool Read(mem_index_t index, size_t offset, T& value);
};

template<typename T>
bool Memory::Read(mem_index_t index, size_t offset, T& value) {
   const Chunk *chunk = Get(index);
   if (chunk == 0) {
      return false;
   }
   /****************************************************************************
      TODO: Check that the offset has the requested type (matches the layout!)
   ****************************************************************************/
   if (offset + sizeof(T) > chunk->size) {
      return false;
   }
   value = *(T*)(data + chunk->start + offset);
   return true;
}


// Stack ///////////////////////////////////////////////////////////////////////

class Stack {
   struct Var {
      name_index_t name;
      mem_index_t  chunk;

      Var(name_index_t _name, mem_index_t _chunk)
         : name(_name), chunk(_chunk) {}
   };
   struct Frame {
      func_index_t func;
      std::vector<Var> locals;

      Frame(func_index_t f) : func(f) {}
   };

   Memory& _memory;
   std::stack<Frame> _frames;

public:
   Stack(Memory& mem) : _memory(mem) {}
   void PushFrame(func_index_t func);
   void PopFrame();
   bool PushLocal(name_index_t name, Type type, size_t& index);
   void PopLocal();
   mem_index_t GetLocal(size_t index);
};

// VM //////////////////////////////////////////////////////////////////////////

class VM {
   NameTable _names;
   TypeTable _types;
   Memory    _memory;
   // Globals _globals;
   Stack     _stack;
};

}

#endif