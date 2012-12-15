from threading import Condition

logFile = None
logFileName = None
logCondition = Condition()
logLineCount = None
logFlushFrequency = None

def openLog(name = "log.txt", flushFrequency = 20):
    global logFile
    global logFileName
    global logLineCount
    global logFlushFrequency
    logFileName = name
    logFile = open(logFileName, "w")
    logLineCount = 0
    logFlushFrequency = flushFrequency

def closeLog():
    global logFile
    if logFile == None: return

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

    # print st                           # uncomment this line during unit testing
    logFile.write(st + "\n")
    logLineCount += 1
    if logLineCount >= logFlushFrequency:
        logFile.close()
        logFile = open(logFileName, "a")
        logLineCount = 0

    logCondition.notify()
    logCondition.release()
    
############################################################################
###################### Unit Testing             ############################
############################################################################

import random
import time
import threading

class PrintLine(threading.Thread):
    def __init__(self, st):
        threading.Thread.__init__(self)
        self.st = st
        self.count = 0
        
    def run(self):
        printLog("Starting " + self.st)
        tempSt = ""
        for i in range(80):
             tempSt = tempSt + self.st
        for i in range(47):
            time.sleep(random.randint(1, 10)/100.0)
            self.count += 1
            printLog(tempSt + ": count = " + str(self.count))
        printLog("Ending " + self.st)
        
if __name__ == "__main__":
    openLog(name = "logzzz.txt", flushFrequency = 10)
    for i in range(9):
        PrintLine(str(i)).start()
    raw_input("Pause until press enter")
    printLog("...................... MIDPOINT.........................")
    flushLog()
    raw_input("Pause until press enter")
    for i in range(10):
        PrintLine(str(i)+str(i)+str(i)).start()
    