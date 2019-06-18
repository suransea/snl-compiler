// Package ast declares the types used to represent syntax trees for SNL
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
// Expressions

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
		Kind     token.Token // token.INTC or token.CHARC
		Value    string      // literal string; e.g. 'a', '\x7f'
	}

	// A ParenExpr node represents a parenthesized expression.
	ParenExpr struct {
		Lparen token.Pos // position of "("
		X      Expr      // parenthesized expression
		Rparen token.Pos // position of ")"
	}

	// A ArrayExpr node represents a array expression.
	ArrayExpr struct {
		Array token.Pos // position of "array"
		Min   int       // range min
		Max   int       // range max
		Type  Expr      // type of array element
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
		Proc   *Ident    // procedure name
		Lparen token.Pos // position of "("
		Args   []Expr    // procedure arguments; or nil
		Rparen token.Pos // position of ")"
	}

	// A BinaryExpr node represents a binary expression.
	BinaryExpr struct {
		X     Expr        // left operand
		OpPos token.Pos   // position of Op
		Op    token.Token // operator
		Y     Expr        // right operand
	}
)

// Pos and End implementations for expression nodes.
func (x *BadExpr) Pos() token.Pos    { return x.From }
func (x *Ident) Pos() token.Pos      { return x.NamePos }
func (x *BasicLit) Pos() token.Pos   { return x.ValuePos }
func (x *ParenExpr) Pos() token.Pos  { return x.Lparen }
func (x *ArrayExpr) Pos() token.Pos  { return x.Array }
func (x *IndexExpr) Pos() token.Pos  { return x.X.Pos() }
func (x *CallExpr) Pos() token.Pos   { return x.Proc.Pos() }
func (x *BinaryExpr) Pos() token.Pos { return x.X.Pos() }

func (x *BadExpr) End() token.Pos    { return x.To }
func (x *Ident) End() token.Pos      { return token.Pos(int(x.NamePos) + len(x.Name)) }
func (x *BasicLit) End() token.Pos   { return token.Pos(int(x.ValuePos) + len(x.Value)) }
func (x *ParenExpr) End() token.Pos  { return x.Rparen + 1 }
func (x *ArrayExpr) End() token.Pos  { return x.Type.End() }
func (x *IndexExpr) End() token.Pos  { return x.Rbrack + 1 }
func (x *CallExpr) End() token.Pos   { return x.Rparen + 1 }
func (x *BinaryExpr) End() token.Pos { return x.Y.End() }

// exprNode() ensures that only expression/type nodes can be
// assigned to an Expr.
//
func (*BadExpr) exprNode()    {}
func (*Ident) exprNode()      {}
func (*BasicLit) exprNode()   {}
func (*ParenExpr) exprNode()  {}
func (*ArrayExpr) exprNode()  {}
func (*IndexExpr) exprNode()  {}
func (*CallExpr) exprNode()   {}
func (*BinaryExpr) exprNode() {}

// A Field represents a Field declaration list in a procedure or var,
//
type Field struct {
	Names []*Ident // field names; or nil
	Type  Expr     // field type
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
	Opening token.Pos // position of opening parenthesis, if any
	List    []*Field  // field list; or nil
	Closing token.Pos // position of closing parenthesis, if any
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
		Decl Decl // *GenDecl with PROCEDURE, TYPE, or VAR token
	}

	// An ExprStmt node represents a (stand-alone) expression
	// in a statement list.
	//
	ExprStmt struct {
		X Expr // expression
	}

	// An EmptyStmt node represents an empty statement.
	// The "position" of the empty statement is the position
	// of the immediately following (explicit or implicit) semicolon.
	//
	EmptyStmt struct {
		Semi token.Pos // position of following ";"
	}

	// An AssignStmt node represents an assignment or
	// a short variable declaration.
	//
	AssignStmt struct {
		Lhs    []Expr
		Assign token.Pos // position of ":="
		Rhs    []Expr
	}

	// A ReturnStmt node represents a return statement.
	ReturnStmt struct {
		Return token.Pos // position of "return" keyword
	}

	// An IfStmt node represents an if statement.
	IfStmt struct {
		If   token.Pos // position of "if" keyword
		Cond Expr      // condition
		Then []Stmt    // body block
		Else []Stmt    // else branch; or nil
		Fi   token.Pos // position of "fi" keyword
	}

	// A WhileStmt represents a while statement.
	WhileStmt struct {
		While token.Pos // position of "while" keyword
		Cond  Expr      // condition; or nil
		Body  Stmt      // body block
		Endwh token.Pos // position of "endwh" keyword
	}
)

// Pos and End implementations for statement nodes.
func (s *BadStmt) Pos() token.Pos    { return s.From }
func (s *DeclStmt) Pos() token.Pos   { return s.Decl.Pos() }
func (s *ExprStmt) Pos() token.Pos   { return s.X.Pos() }
func (s *EmptyStmt) Pos() token.Pos  { return s.Semi }
func (s *AssignStmt) Pos() token.Pos { return s.Lhs[0].Pos() }
func (s *ReturnStmt) Pos() token.Pos { return s.Return }
func (s *IfStmt) Pos() token.Pos     { return s.If }
func (s *WhileStmt) Pos() token.Pos  { return s.While }

func (s *BadStmt) End() token.Pos    { return s.To }
func (s *DeclStmt) End() token.Pos   { return s.Decl.End() }
func (s *ExprStmt) End() token.Pos   { return s.X.End() }
func (s *EmptyStmt) End() token.Pos  { return s.Semi + 1 }
func (s *AssignStmt) End() token.Pos { return s.Rhs[len(s.Rhs)-1].End() }
func (s *ReturnStmt) End() token.Pos { return s.Return + 6 }
func (s *IfStmt) End() token.Pos     { return s.Fi + 2 }
func (s *WhileStmt) End() token.Pos  { return s.Endwh + 5 }

// stmtNode() ensures that only statement nodes can be
// assigned to a Stmt.
//
func (*BadStmt) stmtNode()    {}
func (*DeclStmt) stmtNode()   {}
func (*ExprStmt) stmtNode()   {}
func (*EmptyStmt) stmtNode()  {}
func (*AssignStmt) stmtNode() {}
func (*ReturnStmt) stmtNode() {}
func (*IfStmt) stmtNode()     {}
func (*WhileStmt) stmtNode()  {}

// ----------------------------------------------------------------------------
// Declarations

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

	// A VarDecl node represents vars declaration
	VarDecl struct {
		Var  token.Pos  // position of "var"
		Vars *FieldList // vars
	}

	// A TypeDecl node represents types declaration
	TypeDecl struct {
		Type  token.Pos  // position of "type"
		Types *FieldList // types
	}

	// A ProcDecl node represents a procedure declaration.
	ProcDecl struct {
		Proc   token.Pos  // position of "procedure"
		Name   *Ident     // procedure name
		Params *FieldList // procedure params
		Var    *VarDecl   // procedure var declaration
		Body   []Stmt     // procedure body
		EndPos token.Pos  // position of "end"
	}
)

// Pos and End implementations for declaration nodes.
func (d *BadDecl) Pos() token.Pos  { return d.From }
func (d *VarDecl) Pos() token.Pos  { return d.Var }
func (d *TypeDecl) Pos() token.Pos { return d.Type }
func (d *ProcDecl) Pos() token.Pos { return d.Proc }

func (d *BadDecl) End() token.Pos  { return d.To }
func (d *VarDecl) End() token.Pos  { return d.Vars.End() }
func (d *TypeDecl) End() token.Pos { return d.Types.End() }
func (d *ProcDecl) End() token.Pos { return d.EndPos + 3 }

// declNode() ensures that only declaration nodes can be
// assigned to a Decl.
//
func (*BadDecl) declNode()  {}
func (*VarDecl) declNode()  {}
func (*TypeDecl) declNode() {}
func (*ProcDecl) declNode() {}

// ----------------------------------------------------------------------------
// File

// A file is the root node of an ast
type File struct {
	ProPos token.Pos // position of "program" keyword
	Name   *Ident    // program name
	Decls  []Decl    // top-level declarations; or nil
	Block  []Stmt    // main block
	Dot    token.Pos // position of '.'
}

func (f *File) Pos() token.Pos { return f.ProPos }
func (f *File) End() token.Pos { return f.Dot + 1 }
