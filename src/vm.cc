
#include "vm.hh"
#include "cast.h"

namespace vm {

// TypeTable ///////////////////////////////////////////////////////////////////

size_t TypeTable::SizeOf(Type type) {
  switch (type) {
  case I32: case U32: case F32:
    return 4;
  case I64: case U64: case F64:
    return 8;
  default: {
    TypeDescr *_desc = Get(type);
    switch (_desc->Tag()) {
    case Struct: {
      StructDescr *struc = cast<StructDescr>(_desc);
      size_t size = 0;
      for (int i = 0; i < struc->fields.size(); i++) {
        size += SizeOf(struc->fields[i].type);
      }
      return size;
    }
    case Array: {
      ArrayDescr *array = cast<ArrayDescr>(_desc);
      return array->size * SizeOf(array->elem_type);
    }}
  }}
}

Type TypeTable::Add(TypeDescr *desc) {
  _types.push_back(desc);
  return Type(FirstFreeType + _types.size() - 1);
}

// Memory //////////////////////////////////////////////////////////////////////

Memory::Memory(size_t sz_heap, size_t sz_stack, TypeTable &_tab)
    : _type_table(_tab) 
{
  heap_size = sz_heap;
  total_size = sz_heap + sz_stack;
  stack_pos = sz_heap;
  stack_top = total_size;
  data = (uint8_t *)malloc(total_size);
}

const Memory::Chunk *Memory::Get(ChunkIndex index) const {
  auto it = _chunks.find(index);
  return it != _chunks.end() ? &it->second : (const Chunk *)(0);
}

bool Memory::Alloc(Type type, ChunkIndex &index) {
  size_t size = _type_table.SizeOf(type);
  size_t start = 0;
  auto it = _map.begin();
  for (; it != _map.end(); it++) {
    const Chunk *chunk = Get(it->second);
    if (size < (chunk->start - start)) {
      break;
    }
    start = chunk->start + chunk->size;
  }
  if (it == _map.end() and (heap_size - start) < size) { // Not enough memory!
    return false;
  }

  // Assign new index (never used before)
  index = NewIndex();
  _chunks.insert(std::make_pair(index, Chunk(type, start, size)));
  _map[start] = index;
  return true;
}

void Memory::Free(ChunkIndex index) {
  assert(index < _chunks.size());
  const Chunk *chunk = Get(index);
  _map.erase(chunk->start);
  _chunks.erase(index);
}

bool Memory::StackPush(Type type, ChunkIndex &index) {
  size_t size = _type_table.SizeOf(type);
  if (size > (stack_top - stack_pos)) {
    return false;
  }
  size_t start = stack_pos;
  stack_pos += size;

  index = NewIndex();
  _chunks.insert(std::make_pair(index, Chunk(type, start, size)));
  _map[start] = index;
  _stack.push(index);
  return true;
}

void Memory::StackPop() {
  ChunkIndex index = _stack.top();
  _stack.pop();
  const Chunk *chunk = Get(index);
  stack_pos -= chunk->size;
  _map.erase(chunk->start);
  _chunks.erase(index);
}

// Stack ///////////////////////////////////////////////////////////////////////

void Stack::PushFrame(FuncIndex func) { 
  _frames.push(Frame(func)); 
}

void Stack::PopFrame() { 
  _frames.pop(); 
}

ChunkIndex Stack::GetLocal(size_t index) {
  Frame &current = _frames.top();
  assert(index < current.locals.size());
  return current.locals[index].chunk;
}

bool Stack::PushLocal(NameIndex name, Type type, size_t &index) {
  Frame &current = _frames.top();
  ChunkIndex chunk;
  if (!_memory.StackPush(type, chunk)) {
    return false;
  }
  index = current.locals.size();
  current.locals.push_back(Var(name, chunk));
  return true;
}

void Stack::PopLocal() {
  Frame &current = _frames.top();
  assert(_memory.StackTop() == current.locals.back().chunk);
  current.locals.pop_back();
  _memory.StackPop();
}

// VM //////////////////////////////////////////////////////////////////////////

VM::VM(size_t heap_size, size_t stack_size)
    : _memory(heap_size, stack_size, _types), _stack(_memory) {}

} // namespace vm

