// Package ast declares the types used to represent syntax trees for Go
// packages.
//
package ast

import (
	"snlc/token"
)

// All node types implement the Node interface.
type Node interface {
	Pos() token.Pos // position of first character belonging to the node
	End() token.Pos // position of first character immediately after the node
}

// All expression nodes implement the Expr interface.
type Expr interface {
	Node
	exprNode()
}

// All statement nodes implement the Stmt interface.
type Stmt interface {
	Node
	stmtNode()
}

// All declaration nodes implement the Decl interface.
type Decl interface {
	Node
	declNode()
}

// ----------------------------------------------------------------------------
// Comments

type Comment struct {
	Slash token.Pos // position of "{" starting the comment
	Text  string    // comment text
}

func (c *Comment) Pos() token.Pos { return c.Slash }
func (c *Comment) End() token.Pos { return token.Pos(int(c.Slash) + len(c.Text)) }

// An expression is represented by a tree consisting of one
// or more of the following concrete expression nodes.
//
type (
	// A BadExpr node is a placeholder for expressions containing
	// syntax errors for which no correct expression nodes can be
	// created.
	//
	BadExpr struct {
		From, To token.Pos // position range of bad expression
	}

	// An Ident node represents an identifier.
	Ident struct {
		NamePos token.Pos // identifier position
		Name    string    // identifier name
	}

	// A BasicLit node represents a literal of basic type.
	BasicLit struct {
		ValuePos token.Pos   // literal position
		Kind     token.Token // token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
		Value    string      // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
	}

	// A FuncLit node represents a function literal.
	FuncLit struct {
		Type *FuncType  // function type
		Body *BlockStmt // function body
	}

	// A ParenExpr node represents a parenthesized expression.
	ParenExpr struct {
		Lparen token.Pos // position of "("
		X      Expr      // parenthesized expression
		Rparen token.Pos // position of ")"
	}

	// An IndexExpr node represents an expression followed by an index.
	IndexExpr struct {
		X      Expr      // expression
		Lbrack token.Pos // position of "["
		Index  Expr      // index expression
		Rbrack token.Pos // position of "]"
	}

	// A CallExpr node represents an expression followed by an argument list.
	CallExpr struct {
		Fun      Expr      // function expression
		Lparen   token.Pos // position of "("
		Args     []Expr    // function arguments; or nil
		Ellipsis token.Pos // position of "..." (token.NoPos if there is no "...")
		Rparen   token.Pos // position of ")"
	}

	// A StarExpr node represents an expression of the form "*" Expression.
	// Semantically it could be a unary "*" expression, or a pointer type.
	//
	StarExpr struct {
		Star token.Pos // position of "*"
		X    Expr      // operand
	}

	// A UnaryExpr node represents a unary expression.
	// Unary "*" expressions are represented via StarExpr nodes.
	//
	UnaryExpr struct {
		OpPos token.Pos   // position of Op
		Op    token.Token // operator
		X     Expr        // operand
	}

	// A BinaryExpr node represents a binary expression.
	BinaryExpr struct {
		X     Expr        // left operand
		OpPos token.Pos   // position of Op
		Op    token.Token // operator
		Y     Expr        // right operand
	}

	// A KeyValueExpr node represents (key : value) pairs
	// in composite literals.
	//
	KeyValueExpr struct {
		Key   Expr
		Colon token.Pos // position of ":"
		Value Expr
	}
)

// ----------------------------------------------------------------------------
// Expressions and types

// A Field represents a Field declaration list in a struct type,
// a method list in an interface type, or a parameter/result declaration
// in a signature.
// Field.Names is nil for unnamed parameters (parameter lists which only contain types)
// and embedded struct fields. In the latter case, the field name is the type name.
//
type Field struct {
	Names []*Ident // field/method/parameter names; or nil
	Type  Expr     // field/method/parameter type
}

func (f *Field) Pos() token.Pos {
	if len(f.Names) > 0 {
		return f.Names[0].Pos()
	}
	return f.Type.Pos()
}

func (f *Field) End() token.Pos {
	return f.Type.End()
}

// A FieldList represents a list of Fields, enclosed by parentheses or braces.
type FieldList struct {
	Opening token.Pos // position of opening parenthesis/brace, if any
	List    []*Field  // field list; or nil
	Closing token.Pos // position of closing parenthesis/brace, if any
}

func (f *FieldList) Pos() token.Pos {
	if f.Opening.IsValid() {
		return f.Opening
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if len(f.List) > 0 {
		return f.List[0].Pos()
	}
	return token.NoPos
}

func (f *FieldList) End() token.Pos {
	if f.Closing.IsValid() {
		return f.Closing + 1
	}
	// the list should not be empty in this case;
	// be conservative and guard against bad ASTs
	if n := len(f.List); n > 0 {
		return f.List[n-1].End()
	}
	return token.NoPos
}

// NumFields returns the number of parameters or struct fields represented by a FieldList.
func (f *FieldList) NumFields() int {
	n := 0
	if f != nil {
		for _, g := range f.List {
			m := len(g.Names)
			if m == 0 {
				m = 1
			}
			n += m
		}
	}
	return n
}

// A type is represented by a tree consisting of one
// or more of the following type-specific expression
// nodes.
//
type (
	// An ArrayType node represents an array or slice type.
	ArrayType struct {
		Lbrack token.Pos // position of "["
		Len    Expr      // Ellipsis node for [...]T array types, nil for slice types
		Elt    Expr      // element type
	}

	// Pointer types are represented via StarExpr nodes.

	// A FuncType node represents a function type.
	FuncType struct {
		Func    token.Pos  // position of "func" keyword (token.NoPos if there is no "func")
		Params  *FieldList // (incoming) parameters; non-nil
		Results *FieldList // (outgoing) results; or nil
	}
)

// Pos and End implementations for expression/type nodes.

func (x *BadExpr) Pos() token.Pos      { return x.From }
func (x *Ident) Pos() token.Pos        { return x.NamePos }
func (x *BasicLit) Pos() token.Pos     { return x.ValuePos }
func (x *FuncLit) Pos() token.Pos      { return x.Type.Pos() }
func (x *ParenExpr) Pos() token.Pos    { return x.Lparen }
func (x *IndexExpr) Pos() token.Pos    { return x.X.Pos() }
func (x *CallExpr) Pos() token.Pos     { return x.Fun.Pos() }
func (x *StarExpr) Pos() token.Pos     { return x.Star }
func (x *UnaryExpr) Pos() token.Pos    { return x.OpPos }
func (x *BinaryExpr) Pos() token.Pos   { return x.X.Pos() }
func (x *KeyValueExpr) Pos() token.Pos { return x.Key.Pos() }
func (x *ArrayType) Pos() token.Pos    { return x.Lbrack }
func (x *FuncType) Pos() token.Pos {
	if x.Func.IsValid() || x.Params == nil { // see issue 3870
		return x.Func
	}
	return x.Params.Pos() // interface method declarations have no "func" keyword
}

func (x *BadExpr) End() token.Pos { return x.To }
func (x *Ident) End() token.Pos   { return token.Pos(int(x.NamePos) + len(x.Name)) }

func (x *BasicLit) End() token.Pos     { return token.Pos(int(x.ValuePos) + len(x.Value)) }
func (x *FuncLit) End() token.Pos      { return x.Body.End() }
func (x *ParenExpr) End() token.Pos    { return x.Rparen + 1 }
func (x *IndexExpr) End() token.Pos    { return x.Rbrack + 1 }
func (x *CallExpr) End() token.Pos     { return x.Rparen + 1 }
func (x *StarExpr) End() token.Pos     { return x.X.End() }
func (x *UnaryExpr) End() token.Pos    { return x.X.End() }
func (x *BinaryExpr) End() token.Pos   { return x.Y.End() }
func (x *KeyValueExpr) End() token.Pos { return x.Value.End() }
func (x *ArrayType) End() token.Pos    { return x.Elt.End() }
func (x *FuncType) End() token.Pos {
	if x.Results != nil {
		return x.Results.End()
	}
	return x.Params.End()
}

// exprNode() ensures that only expression/type nodes can be
// assigned to an Expr.
//
func (*BadExpr) exprNode()      {}
func (*Ident) exprNode()        {}
func (*BasicLit) exprNode()     {}
func (*FuncLit) exprNode()      {}
func (*ParenExpr) exprNode()    {}
func (*IndexExpr) exprNode()    {}
func (*CallExpr) exprNode()     {}
func (*StarExpr) exprNode()     {}
func (*UnaryExpr) exprNode()    {}
func (*BinaryExpr) exprNode()   {}
func (*KeyValueExpr) exprNode() {}

func (*ArrayType) exprNode() {}
func (*FuncType) exprNode()  {}

// ----------------------------------------------------------------------------
// Convenience functions for Idents

// NewIdent creates a new Ident without position.
// Useful for ASTs generated by code other than the Go parser.
//
func NewIdent(name string) *Ident { return &Ident{token.NoPos, name} }

func (id *Ident) String() string {
	if id != nil {
		return id.Name
	}
	return "<nil>"
}

// ----------------------------------------------------------------------------
// Statements

// A statement is represented by a tree consisting of one
// or more of the following concrete statement nodes.
//
type (
	// A BadStmt node is a placeholder for statements containing
	// syntax errors for which no correct statement nodes can be
	// created.
	//
	BadStmt struct {
		From, To token.Pos // position range of bad statement
	}

	// A DeclStmt node represents a declaration in a statement list.
	DeclStmt struct {
		Decl Decl // *GenDecl with CONST, TYPE, or VAR token
	}

	// An EmptyStmt node represents an empty statement.
	// The "position" of the empty statement is the position
	// of the immediately following (explicit or implicit) semicolon.
	//
	EmptyStmt struct {
		Semicolon token.Pos // position of following ";"
		Implicit  bool      // if set, ";" was omitted in the source
	}

	// An ExprStmt node represents a (stand-alone) expression
	// in a statement list.
	//
	ExprStmt struct {
		X Expr // expression
	}

	// An AssignStmt node represents an assignment or
	// a short variable declaration.
	//
	AssignStmt struct {
		Lhs    []Expr
		TokPos token.Pos   // position of Tok
		Tok    token.Token // assignment token, DEFINE
		Rhs    []Expr
	}

	// A ReturnStmt node represents a return statement.
	ReturnStmt struct {
		Return  token.Pos // position of "return" keyword
		Results []Expr    // result expressions; or nil
	}

	// A BlockStmt node represents a braced statement list.
	BlockStmt struct {
		Lbrace token.Pos // position of "{"
		List   []Stmt
		Rbrace token.Pos // position of "}"
	}

	// An IfStmt node represents an if statement.
	IfStmt struct {
		If   token.Pos // position of "if" keyword
		Init Stmt      // initialization statement; or nil
		Cond Expr      // condition
		Body *BlockStmt
		Else Stmt // else branch; or nil
	}

	// A ForStmt represents a for statement.
	ForStmt struct {
		For  token.Pos // position of "for" keyword
		Init Stmt      // initialization statement; or nil
		Cond Expr      // condition; or nil
		Post Stmt      // post iteration statement; or nil
		Body *BlockStmt
	}
)

// Pos and End implementations for statement nodes.

func (s *BadStmt) Pos() token.Pos    { return s.From }
func (s *DeclStmt) Pos() token.Pos   { return s.Decl.Pos() }
func (s *EmptyStmt) Pos() token.Pos  { return s.Semicolon }
func (s *ExprStmt) Pos() token.Pos   { return s.X.Pos() }
func (s *AssignStmt) Pos() token.Pos { return s.Lhs[0].Pos() }
func (s *ReturnStmt) Pos() token.Pos { return s.Return }
func (s *BlockStmt) Pos() token.Pos  { return s.Lbrace }
func (s *IfStmt) Pos() token.Pos     { return s.If }
func (s *ForStmt) Pos() token.Pos    { return s.For }

func (s *BadStmt) End() token.Pos  { return s.To }
func (s *DeclStmt) End() token.Pos { return s.Decl.End() }
func (s *EmptyStmt) End() token.Pos {
	if s.Implicit {
		return s.Semicolon
	}
	return s.Semicolon + 1 /* len(";") */
}
func (s *ExprStmt) End() token.Pos   { return s.X.End() }
func (s *AssignStmt) End() token.Pos { return s.Rhs[len(s.Rhs)-1].End() }
func (s *ReturnStmt) End() token.Pos {
	if n := len(s.Results); n > 0 {
		return s.Results[n-1].End()
	}
	return s.Return + 6 // len("return")
}
func (s *BlockStmt) End() token.Pos { return s.Rbrace + 1 }
func (s *IfStmt) End() token.Pos {
	if s.Else != nil {
		return s.Else.End()
	}
	return s.Body.End()
}
func (s *ForStmt) End() token.Pos { return s.Body.End() }

// stmtNode() ensures that only statement nodes can be
// assigned to a Stmt.
//
func (*BadStmt) stmtNode()    {}
func (*DeclStmt) stmtNode()   {}
func (*EmptyStmt) stmtNode()  {}
func (*ExprStmt) stmtNode()   {}
func (*AssignStmt) stmtNode() {}
func (*ReturnStmt) stmtNode() {}
func (*BlockStmt) stmtNode()  {}
func (*IfStmt) stmtNode()     {}
func (*ForStmt) stmtNode()    {}

// ----------------------------------------------------------------------------
// Declarations

// A Spec node represents a single (non-parenthesized) import,
// constant, type, or variable declaration.
//
type (
	// The Spec type stands for any of *ImportSpec, *ValueSpec, and *TypeSpec.
	Spec interface {
		Node
		specNode()
	}

	// A ValueSpec node represents a constant or variable declaration
	// (ConstSpec or VarSpec production).
	//
	ValueSpec struct {
		Names  []*Ident // value names (len(Names) > 0)
		Type   Expr     // value type; or nil
		Values []Expr   // initial values; or nil
	}

	// A TypeSpec node represents a type declaration (TypeSpec production).
	TypeSpec struct {
		Name   *Ident    // type name
		Assign token.Pos // position of '=', if any
		Type   Expr      // *Ident, *ParenExpr, *StarExpr, or any of the *XxxTypes
	}
)

// Pos and End implementations for spec nodes.
func (s *ValueSpec) Pos() token.Pos { return s.Names[0].Pos() }
func (s *TypeSpec) Pos() token.Pos  { return s.Name.Pos() }

func (s *ValueSpec) End() token.Pos {
	if n := len(s.Values); n > 0 {
		return s.Values[n-1].End()
	}
	if s.Type != nil {
		return s.Type.End()
	}
	return s.Names[len(s.Names)-1].End()
}
func (s *TypeSpec) End() token.Pos { return s.Type.End() }

// specNode() ensures that only spec nodes can be
// assigned to a Spec.
//
func (*ValueSpec) specNode() {}
func (*TypeSpec) specNode()  {}

// A declaration is represented by one of the following declaration nodes.
//
type (
	// A BadDecl node is a placeholder for declarations containing
	// syntax errors for which no correct declaration nodes can be
	// created.
	//
	BadDecl struct {
		From, To token.Pos // position range of bad declaration
	}

	// A GenDecl node (generic declaration node) represents an import,
	// constant, type or variable declaration. A valid Lparen position
	// (Lparen.IsValid()) indicates a parenthesized declaration.
	//
	// Relationship between Tok value and Specs element type:
	//
	//	token.VAR     *ValueSpec
	//
	GenDecl struct {
		TokPos token.Pos   // position of Tok
		Tok    token.Token // VAR
		Lparen token.Pos   // position of '(', if any
		Specs  []Spec
		Rparen token.Pos // position of ')', if any
	}

	// A FuncDecl node represents a function declaration.
	FuncDecl struct {
		Name *Ident     // function/method name
		Type *FuncType  // function signature: parameters, results, and position of "func" keyword
		Body *BlockStmt // function body; or nil for external (non-Go) function
	}
)

// Pos and End implementations for declaration nodes.

func (d *BadDecl) Pos() token.Pos  { return d.From }
func (d *GenDecl) Pos() token.Pos  { return d.TokPos }
func (d *FuncDecl) Pos() token.Pos { return d.Type.Pos() }

func (d *BadDecl) End() token.Pos { return d.To }
func (d *GenDecl) End() token.Pos {
	if d.Rparen.IsValid() {
		return d.Rparen + 1
	}
	return d.Specs[0].End()
}
func (d *FuncDecl) End() token.Pos {
	if d.Body != nil {
		return d.Body.End()
	}
	return d.Type.End()
}

// declNode() ensures that only declaration nodes can be
// assigned to a Decl.
//
func (*BadDecl) declNode()  {}
func (*GenDecl) declNode()  {}
func (*FuncDecl) declNode() {}
