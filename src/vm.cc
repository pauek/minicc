
#include "vm.hh"
#include "cast.h"

namespace vm {

// TypeTable ///////////////////////////////////////////////////////////////////

size_t TypeTable::size_of(Type type) {
	size_t size = 0;
	switch (type) {
		case I32:
		case U32:
		case F32:
			size = 4;
			break;
		case I64:
		case U64:
		case F64:
			size = 8;
			break;
		default: {
			TypeDescr *_desc = get(type);
			switch (_desc->Tag()) {
				case Struct: {
					StructDescr *struc = cast<StructDescr>(_desc);
					size = 0;
					for (int i = 0; i < struc->fields.size(); i++) {
						size += size_of(struc->fields[i].type);
					}
					return size;
				}
				case Array: {
					ArrayDescr *array = cast<ArrayDescr>(_desc);
					size = array->size * size_of(array->elem_type);
				}
			}
		}
	}
	return size;
}

Type TypeTable::add(TypeDescr *desc) {
	_types.push_back(desc);
	return Type(FirstFreeType + _types.size() - 1);
}

// Memory //////////////////////////////////////////////////////////////////////

Memory::Memory(size_t sz_heap, size_t sz_stack, TypeTable& _tab) : _type_table(_tab) {
	_heap_size = sz_heap;
	_total_size = sz_heap + sz_stack;
	_stack_pos = sz_heap;
	_stack_top = _total_size;
	_data = (uint8_t *)malloc(_total_size);
}

const Memory::Chunk *Memory::get(ChunkIndex index) const {
	auto it = _chunks.find(index);
	return it != _chunks.end() ? &it->second : (const Chunk *)(0);
}

bool Memory::alloc(Type type, ChunkIndex& index) {
	size_t size = _type_table.size_of(type);
	size_t start = 0;
	auto   it = _map.begin();
	for (; it != _map.end(); it++) {
		const Chunk *chunk = get(it->second);
		if (size < (chunk->start - start)) {
			break;
		}
		start = chunk->start + chunk->size;
	}
	if (it == _map.end() and (_heap_size - start) < size) {	// Not enough memory!
		return false;
	}

	// Assign new index (never used before)
	index = new_index();
	_chunks.insert(std::make_pair(index, Chunk(type, start, size)));
	_map[start] = index;
	return true;
}

void Memory::free(ChunkIndex index) {
	assert(index < _chunks.size());
	const Chunk *chunk = get(index);
	_map.erase(chunk->start);
	_chunks.erase(index);
}

bool Memory::stack_push(Type type, ChunkIndex& index) {
	size_t size = _type_table.size_of(type);
	if (size > (_stack_top - _stack_pos)) {
		return false;
	}
	size_t start = _stack_pos;
	_stack_pos += size;

	index = new_index();
	_chunks.insert(std::make_pair(index, Chunk(type, start, size)));
	_map[start] = index;
	_stack.push(index);
	return true;
}

void Memory::stack_pop() {
	ChunkIndex index = _stack.top();
	_stack.pop();
	const Chunk *chunk = get(index);
	_stack_pos -= chunk->size;
	_map.erase(chunk->start);
	_chunks.erase(index);
}

// Stack ///////////////////////////////////////////////////////////////////////

void Stack::push_frame(FuncIndex func) {
	_frames.push(Frame(func));
}

void Stack::pop_frame() {
	_frames.pop();
}

ChunkIndex Stack::get_local(size_t index) {
	Frame& current = _frames.top();
	assert(index < current.locals.size());
	return current.locals[index].chunk;
}

bool Stack::push_local(NameIndex name, Type type, size_t& index) {
	Frame&	   current = _frames.top();
	ChunkIndex chunk;
	if (!_memory.stack_push(type, chunk)) {
		return false;
	}
	index = current.locals.size();
	current.locals.push_back(Var(name, chunk));
	return true;
}

void Stack::pop_local() {
	Frame& current = _frames.top();
	assert(_memory.stack_top() == current.locals.back().chunk);
	current.locals.pop_back();
	_memory.stack_pop();
}

// VM //////////////////////////////////////////////////////////////////////////

VM::VM(size_t heap_size, size_t stack_size)
	: _memory(heap_size, stack_size, _types), _stack(_memory) {
	for (int i = 0; i < NUM_REGISTERS; i++) {
		_registers[i].i64 = 0L;
	}
}

void VM::test() {
	NameIndex idx1 = _names.put("hola");
	NameIndex idx2 = _names.put("que");
	NameIndex idx3 = _names.put("tal");
}

}  // namespace vm
