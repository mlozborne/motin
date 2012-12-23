############################################################################
###################### Unit Testing             ############################
############################################################################
"""
Expected output:
    Interleaved output from multiple threads
    Each thread logs
        when it is starting
        multiple lines of output
        when it has ended
    There should be NO BLANK LINES in the log and
    each line should contain output from a single thread.
"""
import threading
from Log import *
from time import sleep

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
        for i in range(50):
            self.count += 1
            printLog(tempSt + ": count = " + str(self.count))
#            if i == 20: printLog(1)    # will cause an assertion error
        printLog("Ending " + self.st)

if __name__ == "__main__":
    openLog()
#    openLog(1, 1)          # will cause an assertion error
#    openLog("3", "cat")    # will cause an assertion error
    for i in range(10):
        PrintLine(str(i)).start()
    input("Pause until press enter")
    printLog("...................... MIDPOINT.........................")
    flushLog()
    input("Pause until press enter")
    for i in range(10):
        PrintLine(str(i)+str(i)+str(i)).start()
    input("press enter to quit")
    sleep(2)
    closeLog()