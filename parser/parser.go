package parser

import (
	"fmt"
	"snlc/ast"
	"snlc/scanner"
	"snlc/token"
	"strconv"
)

// The parser structure holds the parser's internal state.
type parser struct {
	file    *token.File
	errors  scanner.ErrorList
	scanner scanner.Scanner

	// Tracing/debugging
	mode   Mode // parsing mode
	trace  bool // == (mode & Trace != 0)
	indent int  // indentation used for tracing output

	// Next token
	pos token.Pos   // token position
	tok token.Token // one token look-ahead
	lit string      // token literal

	// Error recovery
	// (used to limit the number of calls to parser.advance
	// w/o making scanning progress - avoids potential endless
	// loops across multiple parser functions during error recovery)
	syncPos token.Pos // last synchronization position
	syncCnt int       // number of parser.advance calls without progress

	// Non-syntactic parser control
	exprLev int // < 0: in control clause, >= 0: in expression

	unresolved []*ast.Ident // unresolved identifiers

	targetStack [][]*ast.Ident // stack of unresolved labels
}

func (p *parser) init(fset *token.FileSet, filename string, src []byte, mode Mode) {
	p.file = fset.AddFile(filename, -1, len(src))
	var m scanner.Mode
	eh := func(pos token.Position, msg string) { p.errors.Add(pos, msg) }
	p.scanner.Init(p.file, src, eh, m)

	p.mode = mode
	p.trace = mode&Trace != 0 // for convenience (p.trace is used frequently)

	p.next()
}

// ----------------------------------------------------------------------------
// Parsing support

func (p *parser) printTrace(a ...interface{}) {
	const dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
	const n = len(dots)
	pos := p.file.Position(p.pos)
	fmt.Printf("%5d:%3d: ", pos.Line, pos.Column)
	i := 2 * p.indent
	for i > n {
		fmt.Print(dots)
		i -= n
	}
	// i <= n
	fmt.Print(dots[0:i])
	fmt.Println(a...)
}

func trace(p *parser, msg string) *parser {
	p.printTrace(msg, "(")
	p.indent++
	return p
}

// Usage pattern: defer un(trace(p, "..."))
func un(p *parser) {
	p.indent--
	p.printTrace(")")
}

// Advance to the next token.
func (p *parser) next() {
	// Because of one-token look-ahead, print the previous token
	// when tracing as it provides a more readable output. The
	// very first token (!p.pos.IsValid()) is not initialized
	// (it is token.ILLEGAL), so don't print it .
	if p.trace && p.pos.IsValid() {
		s := p.tok.String()
		switch {
		case p.tok.IsLiteral():
			p.printTrace(s, p.lit)
		case p.tok.IsOperator(), p.tok.IsKeyword():
			p.printTrace("\"" + s + "\"")
		default:
			p.printTrace(s)
		}
	}

	p.pos, p.tok, p.lit = p.scanner.Scan()
}

// A bailout panic is raised to indicate early termination.
type bailout struct{}

func (p *parser) error(pos token.Pos, msg string) {
	epos := p.file.Position(pos)

	// If Errors is not set, discard errors reported on the same line
	// as the last recorded error and stop parsing if there are more than
	// 10 errors.
	if p.mode&Errors == 0 {
		n := len(p.errors)
		if n > 0 && p.errors[n-1].Pos.Line == epos.Line {
			return // discard - likely a spurious error
		}
		if n > 10 {
			panic(bailout{})
		}
	}

	p.errors.Add(epos, msg)
}

func (p *parser) errorExpected(pos token.Pos, msg string) {
	msg = "expected " + msg
	if pos == p.pos {
		// the error happened at the current position;
		// make the error message more specific
		switch {
		case p.tok.IsLiteral():
			// print 123 rather than 'INT', etc.
			msg += ", found " + p.lit
		default:
			msg += ", found '" + p.tok.String() + "'"
		}
	}
	p.error(pos, msg)
}

func (p *parser) expect(tok token.Token) token.Pos {
	pos := p.pos
	if p.tok != tok {
		p.errorExpected(pos, "'"+tok.String()+"'")
	}
	p.next() // make progress
	return pos
}

func (p *parser) expectSemi() {
	if p.tok == token.SEMI {
		p.next()
		return
	}
	p.errorExpected(p.pos, "';'")
	p.advance(stmtStart)
}

// advance consumes tokens until the current token p.tok
// is in the 'to' set, or token.EOF. For error recovery.
func (p *parser) advance(to map[token.Token]bool) {
	for ; p.tok != token.EOF; p.next() {
		if to[p.tok] {
			// Return only if parser made some progress since last
			// sync or if it has not reached 10 advance calls without
			// progress. Otherwise consume at least one token to
			// avoid an endless parser loop (it is possible that
			// both parseOperand and parseStmt call advance and
			// correctly do not advance, thus the need for the
			// invocation limit p.syncCnt).
			if p.pos == p.syncPos && p.syncCnt < 10 {
				p.syncCnt++
				return
			}
			if p.pos > p.syncPos {
				p.syncPos = p.pos
				p.syncCnt = 0
				return
			}
			// Reaching here indicates a parser bug, likely an
			// incorrect token list in this function, but it only
			// leads to skipping of possibly correct code if a
			// previous error is present, and thus is preferred
			// over a non-terminating parse.
		}
	}
}

func (p *parser) eqAny(tok ...token.Token) (eq bool, which token.Token) {
	for _, t := range tok {
		if p.tok == t {
			eq, which = true, t
			break
		}
	}
	return
}

var stmtStart = map[token.Token]bool{
	token.WHILE:  true,
	token.DO:     true,
	token.IF:     true,
	token.THEN:   true,
	token.ELSE:   true,
	token.BEGIN:  true,
	token.RETURN: true,
}

var exprEnd = map[token.Token]bool{
	token.COMMA:  true,
	token.SEMI:   true,
	token.RPAREN: true,
	token.RBRACK: true,
}

// safePos returns a valid file position for a given position: If pos
// is valid to begin with, safePos returns pos. If pos is out-of-range,
// safePos returns the EOF position.
//
// This is hack to work around "artificial" end positions in the AST which
// are computed by adding 1 to (presumably valid) token positions. If the
// token positions are invalid due to parse errors, the resulting end position
// may be past the file's EOF position, which would lead to panics if used
// later on.
//
func (p *parser) safePos(pos token.Pos) (res token.Pos) {
	defer func() {
		if recover() != nil {
			res = token.Pos(p.file.Base() + p.file.Size()) // EOF position
		}
	}()
	_ = p.file.Offset(pos) // trigger a panic if position is out-of-range
	return pos
}

// ----------------------------------------------------------------------------
// Identifiers

func (p *parser) parseIdent() *ast.Ident {
	pos := p.pos
	name := "_"
	if p.tok == token.IDENT {
		name = p.lit
		p.next()
	} else {
		p.expect(token.IDENT) // use expect() error handling
	}
	return &ast.Ident{NamePos: pos, Name: name}
}

func (p *parser) parseIdentList() (list []*ast.Ident) {
	if p.trace {
		defer un(trace(p, "IdentList"))
	}

	list = append(list, p.parseIdent())
	for p.tok == token.COMMA {
		p.next()
		list = append(list, p.parseIdent())
	}

	return
}

// ----------------------------------------------------------------------------
// Common productions

func (p *parser) parseExprList() (list []ast.Expr) {
	if p.trace {
		defer un(trace(p, "ExpressionList"))
	}

	list = append(list, p.parseExpr())
	for p.tok == token.COMMA {
		p.next()
		list = append(list, p.parseExpr())
	}

	return
}

// ----------------------------------------------------------------------------
// Statement list

func (p *parser) parseStmtList(end ...token.Token) (list []ast.Stmt, use token.Token) {
	if p.trace {
		defer un(trace(p, "StatementList"))
	}

	eq, use := p.eqAny(end...)

	for !eq && p.tok != token.EOF {
		list = append(list, p.parseStmt())
	}

	return
}

// ----------------------------------------------------------------------------
// Expressions

func (p *parser) isType() (is bool) {
	is, _ = p.eqAny(token.IDENT, token.INTEGER, token.CHAR, token.ARRAY)
	return
}

func (p *parser) parseType() ast.Expr {
	if p.trace {
		defer un(trace(p, "Type"))
	}

	switch p.tok {
	case token.INTEGER, token.CHAR:
		x := &ast.Ident{NamePos: p.pos, Name: p.tok.String()}
		p.next()
		return x
	case token.ARRAY:
		return p.parseArrayExpr()
	case token.IDENT:
		return p.parseIdent()
	default:
		pos := p.pos
		p.errorExpected(p.pos, "type")
		p.advance(exprEnd)
		return &ast.BadExpr{From: pos, To: p.pos}
	}
}

func (p *parser) parseBasicLit() *ast.BasicLit {
	if p.trace {
		defer un(trace(p, "BasicLit"))
	}

	return &ast.BasicLit{ValuePos: p.pos, Kind: p.tok, Value: p.lit}
}

func (p *parser) parseParenExpr() *ast.ParenExpr {
	if p.trace {
		defer un(trace(p, "ParenExpr"))
	}

	lparen := p.expect(token.LPAREN)
	x := p.parseExpr()
	rparen := p.expect(token.RPAREN)

	return &ast.ParenExpr{Lparen: lparen, X: x, Rparen: rparen}
}

func (p *parser) parseArrayExpr() *ast.ArrayExpr {
	if p.trace {
		defer un(trace(p, "ArrayExpr"))
	}

	array := p.expect(token.ARRAY)
	p.expect(token.LBRACK)
	p.expect(token.INTC)
	min, _ := strconv.Atoi(p.lit)
	p.expect(token.UNDERANGE)
	p.expect(token.INTC)
	max, _ := strconv.Atoi(p.lit)
	p.expect(token.RBRACK)
	p.expect(token.OF)
	typ := p.parseType()

	return &ast.ArrayExpr{Array: array, Min: min, Max: max, Type: typ}
}

func (p *parser) parseIndexExpr(x ast.Expr) *ast.IndexExpr {
	if p.trace {
		defer un(trace(p, "IndexExpr"))
	}

	lbrack := p.expect(token.LBRACK)
	index := p.parseExpr()
	rbrack := p.expect(token.RBRACK)

	return &ast.IndexExpr{X: x, Lbrack: lbrack, Index: index, Rbrack: rbrack}
}

func (p *parser) parseCallExpr(proc *ast.Ident) *ast.CallExpr {
	if p.trace {
		defer un(trace(p, "CallExpr"))
	}

	lparen := p.expect(token.LPAREN)
	args := p.parseExprList()
	rparen := p.expect(token.RPAREN)

	return &ast.CallExpr{Proc: proc, Lparen: lparen, Args: args, Rparen: rparen}
}

// If x is of the form (T), unparen returns unparen(T), otherwise it returns x.
func unparen(x ast.Expr) ast.Expr {
	if p, isParen := x.(*ast.ParenExpr); isParen {
		x = unparen(p.X)
	}
	return x
}

func (p *parser) parseBinaryExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "BinaryExpr"))
	}

	x := p.parseExpr()
	var (
		op    token.Token
		opPos token.Pos
	)
	switch p.tok {
	case token.ADD, token.SUB, token.MUL, token.QUO, token.EQL, token.GTR, token.LSS:
		op = p.tok
		opPos = p.pos
	default:
		pos := p.pos
		p.errorExpected(p.pos, "binary operator")
		p.advance(exprEnd)
		return &ast.BadExpr{From: pos, To: p.pos}
	}
	y := p.parseExpr()

	return &ast.BinaryExpr{X: x, Op: op, OpPos: opPos, Y: y}
}

// If lhs is set and the result is an identifier, it is not resolved.
// The result may be a type or even a raw type ([...]int). Callers must
// check the result (using checkExpr or checkExprOrType), depending on
// context.
func (p *parser) parseExpr() ast.Expr {
	if p.trace {
		defer un(trace(p, "Expression"))
	}

	return p.parseBinaryExpr() //TODO
}

// ----------------------------------------------------------------------------
// Statements

func (p *parser) parseAssignStmt(lhs ast.Expr) *ast.AssignStmt {
	if p.trace {
		defer un(trace(p, "AssignStmt"))
	}

	if p.tok == token.LBRACK {
		lhs = p.parseIndexExpr(lhs)
	}
	assign := p.expect(token.ASSIGN)
	rhs := p.parseExpr()

	return &ast.AssignStmt{Lhs: lhs, Assign: assign, Rhs: rhs}
}

func (p *parser) parseReturnStmt() *ast.ReturnStmt {
	if p.trace {
		defer un(trace(p, "ReturnStmt"))
	}

	pos := p.expect(token.RETURN)

	return &ast.ReturnStmt{Return: pos}
}

func (p *parser) parseIfStmt() *ast.IfStmt {
	if p.trace {
		defer un(trace(p, "IfStmt"))
	}

	pos := p.expect(token.IF)
	cond := p.parseExpr()
	p.expect(token.THEN)
	then, use := p.parseStmtList(token.ELSE, token.FI)
	var els []ast.Stmt = nil
	if use == token.ELSE {
		p.expect(token.ELSE)
		els, _ = p.parseStmtList(token.FI)
	}
	fi := p.expect(token.FI)

	return &ast.IfStmt{If: pos, Cond: cond, Then: then, Else: els, Fi: fi}
}

func (p *parser) parseWhileStmt() ast.Stmt {
	if p.trace {
		defer un(trace(p, "WhileStmt"))
	}

	pos := p.expect(token.WHILE)
	cond := p.parseExpr()
	p.expect(token.DO)
	body, _ := p.parseStmtList(token.ENDWH)
	endwh := p.expect(token.ENDWH)

	return &ast.WhileStmt{While: pos, Cond: cond, Body: body, Endwh: endwh}
}

func (p *parser) parseStmt() (s ast.Stmt) {
	if p.trace {
		defer un(trace(p, "Statement"))
	}

	switch p.tok {
	case token.PROCEDURE:
		s = &ast.DeclStmt{Decl: p.parseProcDecl()}
	case token.IDENT:
		id := p.parseIdent()
		switch p.tok {
		case token.ASSIGN:
			s = p.parseAssignStmt(id)
		default:
			s = p.parseStmt()
		}
	case
		token.INTC, token.CHARC, token.LPAREN,
		token.LBRACK, token.ADD, token.SUB:
		s = &ast.ExprStmt{X: p.parseExpr()}
		p.expectSemi()
	case token.RETURN:
		s = p.parseReturnStmt()
	case token.IF:
		s = p.parseIfStmt()
		if p.tok == token.SEMI {
			p.expectSemi()
		}
	case token.WHILE:
		s = p.parseWhileStmt()
	case token.SEMI:
		s = &ast.EmptyStmt{Semi: p.pos}
		p.next()
	default:
		// no statement found
		pos := p.pos
		p.errorExpected(pos, "statement")
		p.advance(stmtStart)
		s = &ast.BadStmt{From: pos, To: p.pos}
	}

	return
}

// ----------------------------------------------------------------------------
// Declarations

func (p *parser) isDecl() (is bool) {
	is, _ = p.eqAny(token.TYPE, token.VAR, token.PROCEDURE)
	return
}

func (p *parser) parseVarList() *ast.FieldList {
	if p.trace {
		defer un(trace(p, "VarList"))
	}

	var list []*ast.Field

	for p.isType() {
		typ := p.parseType()
		ids := p.parseIdentList()
		list = append(list, &ast.Field{Names: ids, Type: typ})
		p.expectSemi()
	}

	return &ast.FieldList{List: list}
}

func (p *parser) parseTypeList() *ast.FieldList {
	if p.trace {
		defer un(trace(p, "TypeList"))
	}

	var list []*ast.Field

	for p.tok == token.IDENT {
		id := p.parseIdent()
		p.expect(token.EQL)
		typ := p.parseType()
		list = append(list, &ast.Field{Names: []*ast.Ident{id}, Type: typ})
		p.expectSemi()
	}

	return &ast.FieldList{List: list}
}

func (p *parser) parseParamList() *ast.FieldList {
	if p.trace {
		defer un(trace(p, "ParamList"))
	}

	var list []*ast.Field

	lp := p.expect(token.LPAREN)
	first := p.pos
	for p.tok != token.RPAREN {
		if p.pos != first {
			p.expectSemi()
			p.expect(token.VAR)
		}
		typ := p.parseType()
		ids := p.parseIdentList()
		list = append(list, &ast.Field{Names: ids, Type: typ})
	}
	rp := p.expect(token.RPAREN)

	return &ast.FieldList{Lparen: lp, List: list, Rparen: rp}
}

func (p *parser) parseVarDecl() *ast.VarDecl {
	if p.trace {
		defer un(trace(p, "VarDecl"))
	}

	pos := p.expect(token.VAR)
	vars := p.parseVarList()
	return &ast.VarDecl{Var: pos, Vars: vars}
}

func (p *parser) parseTypeDecl() *ast.TypeDecl {
	if p.trace {
		defer un(trace(p, "TypeDecl"))
	}

	pos := p.expect(token.TYPE)
	types := p.parseTypeList()
	return &ast.TypeDecl{Type: pos, Types: types}
}

func (p *parser) parseProcDecl() *ast.ProcDecl {
	if p.trace {
		defer un(trace(p, "ProcDecl"))
	}

	proc := p.expect(token.PROCEDURE)
	name := p.parseIdent()
	params := p.parseParamList()
	p.expectSemi()
	vars := p.parseVarDecl()
	p.expect(token.BEGIN)
	body, _ := p.parseStmtList(token.END)
	end := p.expect(token.END)

	return &ast.ProcDecl{
		Proc:   proc,
		Name:   name,
		Params: params,
		Var:    vars,
		Body:   body,
		EndPos: end,
	}
}

func (p *parser) parseDecl() ast.Decl {
	if p.trace {
		defer un(trace(p, "Declaration"))
	}

	switch p.tok {
	case token.VAR:
		return p.parseVarDecl()
	case token.TYPE:
		return p.parseTypeDecl()
	case token.PROCEDURE:
		return p.parseProcDecl()
	default:
		pos := p.pos
		p.errorExpected(p.pos, "declaration")
		p.advance(stmtStart)
		return &ast.BadDecl{From: pos, To: p.pos}
	}
}

// ----------------------------------------------------------------------------
// Source files

func (p *parser) parseFile() *ast.File {
	if p.trace {
		defer un(trace(p, "File"))
	}

	// Don't bother parsing the rest if we had errors scanning the first token.
	// Likely not a Go source file at all.
	if p.errors.Len() != 0 {
		return nil
	}

	// program clause
	pos := p.expect(token.PROGRAM)
	ident := p.parseIdent()
	if ident.Name == "_" && p.mode&Errors != 0 {
		p.error(p.pos, "invalid program name _")
	}

	// Don't bother parsing the rest if we had errors parsing the program clause.
	// Likely not a SNL source file at all.
	if p.errors.Len() != 0 {
		return nil
	}

	var decls []ast.Decl

	for p.isDecl() {
		decls = append(decls, p.parseDecl())
	}

	p.expect(token.BEGIN)
	body, _ := p.parseStmtList(token.END)
	p.expect(token.END)
	dot := p.expect(token.DOT)

	return &ast.File{
		Prog:  pos,
		Name:  ident,
		Decls: decls,
		Body:  body,
		Dot:   dot,
	}
}
