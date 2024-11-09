#ifndef VALUE_HH
#define VALUE_HH

#include <cstring>
#include "ast.hh"
#include "util.hh"

/*

A value can be:
1) null:      _box = 0.
2) 'astract': _box != 0   and   _box->data == 0.
3) normal:    _box != 0   and   (_box->type != 0 and _box->data != 0).

Abstract values are used to represent types (so that we can reuse
environments for static analysis).

 */

struct Type;

class Value {  // new value

	struct Box {
		int			count;
		const Type *type;
		void	   *data;
		bool		touched;

		Box() : count(0), type(0), data(0), touched(false) {}

		Box(const Type *t, void *d) : count(0), type(t), data(d), touched(d != 0) {}
	};

	Box *_box;
	bool _const;

	void _detach(Box *b);
	void _attach(Box *b);

	explicit Value(Box *box, bool cnst);

   public:
	static void *abstract, *unknown;

	explicit Value() : _box(0), _const(false) {}

	explicit Value(const Type *t,
				   void		  *d = 0,
				   bool		   cnst = false);	// with data = 0 creates an 'abstract' Value
	Value(const Value& v);

	explicit Value(int x);
	explicit Value(char x);
	explicit Value(bool x);
	explicit Value(float x);
	explicit Value(double x);
	explicit Value(std::string x);
	explicit Value(const char *x);	// string!
	explicit Value(std::ostream& o);
	explicit Value(std::istream& i);

	~Value();

	const Type *type() const { return (_box == 0 ? 0 : _box->type); }

	void *data() const { return (_box == 0 ? 0 : _box->data); }

	void touch();

	bool is_const() const { return _const; }

	void set_const(bool cnst) { _const = cnst; }

	void clear_touched();

	template <typename TestClass>
	bool is() const;
	template <typename TestClass>
	typename TestClass::cpp_type& as();
	template <typename TestClass>
	typename TestClass::cpp_type& as() const;

	bool has_type(const Type *t) const { return _box->type == t; }

	static Value null;

	bool is_null() const { return _box == 0; }

	bool is_abstract() const { return _box != 0 and _box->data == abstract; }

	bool is_unknown() const { return _box != 0 and _box->data == unknown; }

	bool is_concrete() const {
		return _box != 0 and (_box->data != unknown and _box->data != abstract);
	}

	void to_abstract() const {
		assert(_box != 0);
		_box->data = abstract;
	}

	bool contains_unknowns() const;

	std::string type_name() const;

	bool same_type_as(const Value& v) const;

	// This means "it is the same object" (the same Box), like in Java
	const bool operator==(const Value& v) const { return _box == v._box; }

	// Comparison of data
	bool equals(const Value& v) const;
	bool less_than(const Value& other) const;

	bool operator<(const Value& v) const { return less_than(v); }

	const Value& operator=(const Value& v);	 // copies reference, not Box!
	bool		 assign(const Value& v);	 // copies content of Box
	Value		 clone() const;

	void		write(std::ostream& o) const;
	void		read(std::istream& i);
	std::string to_json() const;

	friend class Reference;
};

std::ostream& operator<<(std::ostream& o, const Value& v);
std::istream& operator>>(std::istream& o, Value& v);

#endif
