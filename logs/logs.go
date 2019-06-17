package logs

var (
	infoLogger, debugLogger, errorLogger = NewLogger()
)

func Info(v ...interface{}) {
	infoLogger.Println(v)
}

func Debug(v ...interface{}) {
	debugLogger.Println(v)
}

func Error(v ...interface{}) {
	errorLogger.Println(v)
}
