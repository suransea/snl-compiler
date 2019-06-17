package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"snlc/logs"
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
		logs.Error("no args provided.")
		os.Exit(1)
	}

	// print tokens
	if tokens {
		for _, arg := range flag.Args() {
			bytes, err := ioutil.ReadFile(arg)
			if err != nil {
				logs.Error(err)
				os.Exit(1)
			}
			scan(arg, bytes)
		}
	}

	// generate ast
	if tree {
		return
	}
}

// scan tokens from bytes with filename
func scan(filename string, bytes []byte) {
	s := new(scanner.Scanner)
	fset := token.NewFileSet()
	file := fset.AddFile(filename, fset.Base(), len(bytes))
	errs := new(scanner.ErrorList)
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
