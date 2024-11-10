
#ifndef VM_HH
#define VM_HH
#include <cassert>
#include <cstdint>
#include <map>
#include <stack>
#include <string>
#include <vector>

namespace vm {
typedef uint32_t Type;
typedef uint32_t ChunkIndex;
typedef size_t   NameIndex;
typedef size_t   FuncIndex;

struct Pointer {
    ChunkIndex index;
    size_t     offset;
};

// Types ///////////////////////////////////////////////////////////////////////
enum BasicType { I32, U32, F32, I64, U64, F64, FirstFreeType };

enum CompositeType { Struct, Array };

struct TypeDescr {   // The layout description of a compound type
    NameIndex name;  // index to the name table

    CompositeType Tag() const { return _tag; }

   protected:
    CompositeType _tag;
};

template <CompositeType T>
struct TypeDescrDerived : TypeDescr {
    static bool is_instance(const TypeDescr *desc) { return desc->Tag() == T; }

    TypeDescrDerived() { _tag = T; }
};

struct Field {
    NameIndex name;
    Type      type;
};

struct StructDescr : TypeDescrDerived<Struct> {
    std::vector<Field> fields;

    StructDescr(NameIndex _name, const std::vector<Field>& _fields) {
        name = _name;
        fields = _fields;
    }
};

struct ArrayDescr : TypeDescrDerived<Array> {
    size_t size;
    Type   elem_type;

    ArrayDescr(NameIndex _name, size_t _size, Type _elem_type)
        : size(_size), elem_type(_elem_type) {
        name = _name;
    }
};

// TypeTable ///////////////////////////////////////////////////////////////////
class TypeTable {
    // The first type numbers are used for basic types so we have to subtract
    // FirstFreeType always from the index (or add it in the add method)
    std::vector<TypeDescr *> _types;

   public:
    TypeDescr *get(Type index) {
        assert(index >= FirstFreeType && (index - FirstFreeType) < _types.size());
        return _types[(size_t)index - FirstFreeType];
    }

    size_t size_of(Type type);
    Type   add(TypeDescr *desc);
};

// NameTable ///////////////////////////////////////////////////////////////////
class NameTable {
    std::vector<std::string> _names;

   public:
    size_t put(std::string name) {
        for (size_t i = 0; i < _names.size(); i++) {
            if (name == _names[i]) {
                return i;
            }
        }
        size_t index = _names.size();
        _names.push_back(name);
        return index;
    }

    std::string get(size_t index) {
        assert(index < _names.size());
        return _names[index];
    }
};

// Memory //////////////////////////////////////////////////////////////////////
const size_t MiB = 1024 * 1024;

class Memory {
    struct Chunk {
        Type   type;
        size_t start, size;

        Chunk(Type _type, size_t _start, size_t _size) : type(_type), start(_start), size(_size) {}
    };

    TypeTable& _type_table;
    uint8_t   *_data;
    size_t     _total_size, _heap_size, _stack_pos, _stack_top;
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
    ChunkIndex                   last = 0;  // Indices keep growing
    std::map<ChunkIndex, Chunk>  _chunks;
    std::map<size_t, ChunkIndex> _map;
    std::stack<ChunkIndex>       _stack;  // stack of indices to chunks

    ChunkIndex new_index() { return ++last; }

   public:
    Memory(size_t sz_heap, size_t sz_stack, TypeTable& type_table);
    bool alloc(Type t, ChunkIndex& index);
    void free(ChunkIndex);
    bool stack_push(Type t, ChunkIndex& index);

    ChunkIndex stack_top() const { return _stack.top(); }

    void         stack_pop();
    const Chunk *get(ChunkIndex index) const;
    template <typename TestClass>
    bool read(ChunkIndex index, size_t offset, TestClass& value);
};

template <typename TestClass>
bool Memory::read(ChunkIndex index, size_t offset, TestClass& value) {
    const Chunk *chunk = get(index);
    if (chunk == 0) {
        return false;
    }
    /****************************************************************************
         TODO: Check that the offset has the requested type (matches the layout!)
    ****************************************************************************/
    if (offset + sizeof(TestClass) > chunk->size) {
        return false;
    }
    value = *(TestClass *)(_data + chunk->start + offset);
    return true;
}

// Stack ///////////////////////////////////////////////////////////////////////
class Stack {
    struct Var {
        NameIndex  name;
        ChunkIndex chunk;

        Var(NameIndex _name, ChunkIndex _chunk) : name(_name), chunk(_chunk) {}
    };

    struct Frame {
        FuncIndex        func;
        std::vector<Var> locals;

        Frame(FuncIndex f) : func(f) {}
    };

    Memory&           _memory;
    std::stack<Frame> _frames;

   public:
    Stack(Memory& mem) : _memory(mem) {}

    void       push_frame(FuncIndex func);
    void       pop_frame();
    bool       push_local(NameIndex name, Type type, size_t& index);
    void       pop_local();
    ChunkIndex get_local(size_t index);
};

// Registers ///////////////////////////////////////////////////////////////////
union Register {
    int32_t  i32;
    uint32_t u32;
    float    f32;
    int64_t  i64;
    uint64_t u64;
    double   f64;
};

const size_t     NUM_REGISTERS = 32;
typedef Register Registers[NUM_REGISTERS];

// VM //////////////////////////////////////////////////////////////////////////
class VM {
    NameTable _names;
    TypeTable _types;
    Memory    _memory;
    Stack     _stack;
    Registers _registers;
    // Globals _globals;
   public:
    VM(size_t heap_size = 16 * MiB, size_t stack_size = 4 * MiB);
    void test();
};
}  // namespace vm
#endif