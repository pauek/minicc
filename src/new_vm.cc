
#include "new_types.hh"
#include "new_vm.hh"

namespace vm {

Memory::Memory(size_t size, TypeTable& _tab) 
   : _type_table(_tab)
{
   data = malloc(size);
   total_size = size;
}

const Memory::Chunk* Memory::Get(mem_index_t index) const {
   auto it = _chunks.find(index);
   return it != _chunks.end() ? &it->second : (const Chunk *)(0);
}

bool Memory::Alloc(Type type, mem_index_t& index) {
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
   if (it == _map.end() and (total_size - start) < size) {  // Not enough memory!
      return false;
   }

   // Assign new index (never used before)
   index = last + 1;
   last = index;
   _chunks.insert(std::make_pair(index, Chunk(type, start, size)));
   _map[start] = index;
   return true;
}

void Memory::Free(mem_index_t index) {
   assert(index < _chunks.size());
   const Chunk *chunk = Get(index);
   _map.erase(chunk->start);
   _chunks.erase(index);
}

}