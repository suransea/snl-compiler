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

	// print tokens
	if tokens {
		for i, arg := range flag.Args() {
			bytes, err := ioutil.ReadFile(arg)
			if err != nil {
				logs.Error("file", i, "name", arg, err)
				os.Exit(1)
			}
			scan(arg, bytes)
		}
	}

	// generate ast
	if tree {
		for _, arg := range flag.Args() {
			parse(arg)
		}
	}
}

// scan bytes with filename and print the tokens
func scan(filename string, bytes []byte) {
	fset := token.NewFileSet()
	file := fset.AddFile(filename, fset.Base(), len(bytes))
	var errs scanner.ErrorList
	eh := func(pos token.Position, msg string) { errs.Add(pos, msg) }
	var s scanner.Scanner
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
func parse(filename string) {
	fset := token.NewFileSet()
	tree, err := parser.ParseFile(fset, filename, nil, 0)
	if err != nil {
		if errs, ok := err.(scanner.ErrorList); ok {
			logs.Info("[syntax]", "error count:", errs.Len())
		}
		//scanner.PrintError(os.Stderr, err)
	}
	err = ast.Print(fset, tree)
	if err != nil {
		logs.Error(err)
	}
}
