package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"snlc/ast"
	"snlc/logs"
	"snlc/parser"
	"snlc/scanner"
	"snlc/token"
)

var (
	tokens, tree bool // CLI args
)

func init() {
	// bind CLI args
	flag.BoolVar(&tokens, "tokens", false, "print tokens")
	flag.BoolVar(&tree, "tree", false, "generate ast")
}

func main() {
	flag.Parse()
	if len(flag.Args()) == 0 {
		logs.Error("no file arg provided.")
		os.Exit(1)
	}

	for i, arg := range flag.Args() {
		bytes, err := ioutil.ReadFile(arg)
		if err != nil {
			logs.Error("file", i, "name", arg, err)
			os.Exit(1)
		}
		switch {
		case tokens:
			scan(arg, bytes) // print tokens
		case tree:
			parse(arg, bytes) // generate ast
		}
	}
}

// scan bytes with filename and print the tokens
func scan(filename string, bytes []byte) {
	var s scanner.Scanner
	var errs scanner.ErrorList
	fset := token.NewFileSet()
	file := fset.AddFile(filename, fset.Base(), len(bytes))
	eh := func(pos token.Position, msg string) { errs.Add(pos, msg) }
	s.Init(file, bytes, eh, scanner.ScanComments)
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}
		fmt.Printf("%s\t%s\t%q\n", fset.Position(pos), tok, lit)
	}
	logs.Info("[lexical]", "error count:", errs.Len())
	if errs.Len() > 0 {
		scanner.PrintError(os.Stderr, errs)
	}
}

// parse bytes with filename and print the ast
func parse(filename string, bytes []byte) {
	fset := token.NewFileSet()
	fset.AddFile(filename, fset.Base(), len(bytes))
	tree, err := parser.ParseFile(fset, filename, bytes, 0)
	if err != nil {
		logs.Error(err)
	}
	err = ast.Print(fset, tree)
	if err != nil {
		logs.Error(err)
	}
}
