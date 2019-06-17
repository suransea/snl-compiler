package logs

import (
	"log"
	"os"
)

func NewInfoLogger() *log.Logger {
	return log.New(os.Stdout, "[Info] ", log.LstdFlags)
}

func NewDebugLogger() *log.Logger {
	return log.New(os.Stdout, "[Debug] ", log.LstdFlags|log.Lmicroseconds)
}

func NewErrorLogger() *log.Logger {
	return log.New(os.Stderr, "[Error] ", log.LstdFlags|log.Lmicroseconds)
}

func NewLogger() (info *log.Logger, debug *log.Logger, error *log.Logger) {
	info = NewInfoLogger()
	debug = NewDebugLogger()
	error = NewErrorLogger()
	return
}
