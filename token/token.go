package token

import (
	"strconv"
)

// Token is the set of lexical tokens of the SNL programming language.
type Token int

// The list of tokens.
// noinspection GoSnakeCaseUsage
const (
	// Special tokens
	ILLEGAL Token = iota
	EOF
	COMMENT

	literal_beg
	// Identifiers and basic type literals
	// (these tokens stand for classes of literals)
	IDENT // main
	INTC  // 12345
	CHARC // 'a'
	literal_end

	operator_beg
	// Operators and delimiters
	ADD // +
	SUB // -
	MUL // *
	QUO // /

	EQL    // =
	LSS    // <
	GTR    // >
	ASSIGN // :=

	UNDERANGE // ..

	LPAREN // (
	LBRACK // [
	LBRACE // {
	COMMA  // ,
	DOT    // .

	RPAREN // )
	RBRACK // ]
	RBRACE // }
	SEMI   // ;
	operator_end

	keyword_beg
	// Keywords
	PROGRAM
	PROCEDURE
	TYPE
	VAR
	IF
	THEN
	ELSE
	FI
	WHILE
	DO
	ENDWH
	BEGIN
	END
	READ
	WRITE
	ARRAY
	CHAR
	INTEGER
	OF
	RECORD
	RETURN
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",

	EOF:     "EOF",
	COMMENT: "COMMENT",

	IDENT: "IDENT",
	INTC:  "INTC",
	CHARC: "CHARC",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	QUO: "/",

	EQL:    "=",
	LSS:    "<",
	GTR:    ">",
	ASSIGN: ":=",

	LPAREN: "(",
	LBRACK: "[",
	LBRACE: "{",
	COMMA:  ",",
	DOT:    ".",

	UNDERANGE: "..",

	RPAREN: ")",
	RBRACK: "]",
	RBRACE: "}",
	SEMI:   ";",

	PROGRAM:   "program",
	PROCEDURE: "procedure",
	TYPE:      "type",
	VAR:       "var",
	IF:        "if",
	THEN:      "then",
	ELSE:      "else",
	FI:        "fi",
	WHILE:     "while",
	DO:        "do",
	ENDWH:     "endwh",
	BEGIN:     "begin",
	END:       "end",
	READ:      "read",
	WRITE:     "write",
	ARRAY:     "array",
	CHAR:      "char",
	INTEGER:   "integer",
	OF:        "of",
	RECORD:    "record",
	RETURN:    "return",
}

// String returns the string corresponding to the token tok.
// For operators, delimiters, and keywords the string is the actual
// token character sequence (e.g., for the token ADD, the string is
// "+"). For all other tokens the string corresponds to the token
// constant name (e.g. for the token IDENT, the string is "IDENT").
//
func (tok Token) String() string {
	s := ""
	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}
	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}
	return s
}

// A set of constants for precedence-based expression parsing.
// Non-operators have lowest precedence, followed by operators
// starting with precedence 1 up to unary operators. The highest
// precedence serves as "catch-all" precedence for selector,
// indexing, and other operator and delimiter tokens.
//
const (
	LowestPrec = 0 // non-operators
)

// Precedence returns the operator precedence of the binary
// operator op. If op is not a binary operator, the result
// is LowestPrecedence.
//
func (tok Token) Precedence() int {
	switch tok {
	case EQL, LSS, GTR:
		return 3
	case ADD, SUB:
		return 4
	case MUL, QUO:
		return 5
	}
	return LowestPrec
}

var keywords map[string]Token

func init() {
	keywords = make(map[string]Token)
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

// Lookup maps an identifier to its keyword token or IDENT (if not a keyword).
//
func Lookup(ident string) Token {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	return IDENT
}

// Predicates

// IsLiteral returns true for tokens corresponding to identifiers
// and basic type literals; it returns false otherwise.
//
func (tok Token) IsLiteral() bool { return literal_beg < tok && tok < literal_end }

// IsOperator returns true for tokens corresponding to operators and
// delimiters; it returns false otherwise.
//
func (tok Token) IsOperator() bool { return operator_beg < tok && tok < operator_end }

// IsKeyword returns true for tokens corresponding to keywords;
// it returns false otherwise.
//
func (tok Token) IsKeyword() bool { return keyword_beg < tok && tok < keyword_end }
