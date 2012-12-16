############################################################################
###################### Unit Testing             ############################
############################################################################

import threading
from Log import *

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
    raw_input("press enter to quit")