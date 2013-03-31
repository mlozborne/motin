"""
A log is a thread safe text file.
A process can open at most one log.
If a process attempts to open a second log before closing the first log, nothing happens.
If a process has multiple threads, all threads share the same log.
If a process doesn't open a log, then it's printLog commands are ignored.
Usage
    from Log import openLog, closeLog, flushLog, printLog
    openLog(name = <string>, flushFrequency = <positive integer>)
    printLog(<string>)
    flushLog()
    closeLog()
Where
    o The name of a log file = "log_" + name + ".txt"
    o flushFrequence indicates how many lines are sent to the log before the log
      is closed and reopened, thus allowing users to observe changes to the log
      file.
    o flushLog() closes and reopens the log file
"""
from threading import Condition

logFile = None                         # file object
logFileName = None                     # name of log file
logCondition = Condition()             # concurreny control
logLineCount = None                    # number of lines since last flush
logFlushFrequency = None               # number of lines between flushes

def openLog(name = "1", flushFrequency = 1):
    
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    
    if logFile != None: return

    assert(isinstance(name, str))
    assert(isinstance(flushFrequency, int) and flushFrequency > 0)
    
    logFileName = "log_" + str(name) + ".txt"
    logFile = open(logFileName, "w")
    logLineCount = 0
    logFlushFrequency = flushFrequency

    printLog("Opened " + logFileName)

def closeLog():
    
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    
    if logFile == None: return

    printLog("Closing " + logFileName)

    logFile.close()
    logFile = None

def flushLog():
    
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    
    if logFile == None: return

    logCondition.acquire()
    logFile.close()
    logFile = open(logFileName, "a")
    logLineCount = 0

    logCondition.notify()
    logCondition.release()

def printLog(st):
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    
    if logFile == None: return

    assert(isinstance(st, str))

    logCondition.acquire()

    logFile.write(st + "\n")
    logLineCount += 1
    if logLineCount >= logFlushFrequency:
        logFile.close()
        logFile = open(logFileName, "a")
        logLineCount = 0

    logCondition.notify()
    logCondition.release()
    