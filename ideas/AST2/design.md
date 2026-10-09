# AST Types

```
Expr
	AtomicExpr
		Literal
			BoolLiteral
			CharLiteral
			IntLiteral
			DoubleLiteral
			StringLiteral
		Identifier
	UnaryExpr
		SignExpr
		IncrExpr
		NotExpr
	BinaryExpr
		Multiplicative
        Additive
        Comparison
        LogicalAnd
        LogicalOr
	CallExpr
	FieldExpr
	CondExpr
Stmt
	DeclStmt
	ExprStmt
	IfStmt
	ForStmt
	WhileStmt
	Block
Decl
	ParamDecl
	VarDecl
	UsingDecl
	StructDecl
	FunctionDecl
Preprocessor
	Include
```

The base class of AstNode can add common data:

- `parent`: parent node.
- Comments (representation?)
- token span: `begin`, `end`. (if coming from parsing)

Each node is a `struct` with basic fields, including references to other `T`s
(**not** pointers).

**Stores**: A class `Store<struct T>` stores all instances of `T`. A `Ref<T>` is an
index into `Store<T>`. This way the different nodes can point to each other.

# Operations that the parser needs

- `Store<T>::create_node`: create a node of a certain type.
- Access `struct T` fields directly.

# Walking

- `node.type`: Determining the type of a node.
- `
