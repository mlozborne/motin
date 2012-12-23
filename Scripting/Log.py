from threading import Condition

logFile = None
logFileName = None
logCondition = Condition()
logLineCount = None
logFlushFrequency = None

def openLog(name, flushFrequency):
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    logFileName = "log_" + name + ".txt"
    logFile = open(logFileName, "w")
    logLineCount = 0
    logFlushFrequency = flushFrequency
    printLog("Opening " + logFileName)

def closeLog():
    global logFile
    if logFile == None: return

    printLog("Closing " + logFileName)
    logFile.close()
    logFile = None

def flushLog():
    global logFile
    if logFile == None: return

    logCondition.acquire()
    logFile.close()
    logFile = open(logFileName, "a")

    logCondition.notify()
    logCondition.release()

def printLog(st):
    global logLineCount
    global logFile
    if logFile == None: return

    logCondition.acquire()

    logFile.write(st + "\n")
    logLineCount += 1
    if logLineCount >= logFlushFrequency:
        logFile.close()
        logFile = open(logFileName, "a")
        logLineCount = 0

    logCondition.notify()
    logCondition.release()
    

    