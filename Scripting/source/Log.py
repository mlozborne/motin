"""
A log is a thread safe text file.
A process can open at most one log.
If a process attempts to open a second log before closing the first log, nothing happens.
If a process has multiple threads, all threads share the same log.
If a process doesn't open a log, then it's print commands are ignored.
Usage
    from Log import openLog, closeLog, flushLog, print
    openLog(name = <string>, flushFrequency = <positive integer>)
    print(<string>)
    flushLog()
    closeLog()
Where
    o The name of a log file = "log_" + name + ".txt"
    o flushFrequency indicates how many lines are sent to the log before the log
      is closed and reopened, thus allowing users to observe changes to the log
      file.
    o flushLog() closes and reopens the log file
"""
from threading import Condition
from datetime import datetime


class Log:
    def __init__(self):
        self.logFile = None  # file object
        self.logFileName = None  # name of log file
        self.logCondition = Condition()  # concurrency control
        self.logLineCount = None  # number of lines since last flush
        self.logFlushFrequency = None  # number of lines between flushes

    def open(self, fileName="1", flushFrequency=1):
        if self.logFile is not None:
            return

        assert (isinstance(fileName, str))  # name is a string
        assert (isinstance(flushFrequency, int) and flushFrequency > 0)  # flushFrequency is a positive integer

        self.logFileName = "log_" + str(fileName) + ".txt"
        self.logFile = open(self.logFileName, "w")
        self.logLineCount = 0
        self.logFlushFrequency = flushFrequency

        self.print("Opened " + self.logFileName + " at " + str(datetime.today()))

    def close(self):
        if self.logFile is None:
            return

        # noinspection PyTypeChecker
        self.print("Closing " + self.logFileName + " at " + str(datetime.today()))

        self.logFile.close()
        self.logFile = None

    def flush(self):
        if self.logFile is None:
            return

        self.logCondition.acquire()
        self.logFile.close()
        # noinspection PyTypeChecker
        self.logFile = open(self.logFileName, "a")
        self.logLineCount = 0

        self.logCondition.notify()
        self.logCondition.release()

    def print(self, printStr):
        if self.logFile is None:
            return

        assert(isinstance(printStr, str))

        self.logCondition.acquire()

        self.logFile.write(printStr + "\n")
        self.logLineCount += 1
        if self.logLineCount >= self.logFlushFrequency:
            self.logFile.close()
            # noinspection PyTypeChecker
            self.logFile = open(self.logFileName, "a")
            self.logLineCount = 0

        self.logCondition.notify()
        self.logCondition.release()

#########################################

gLog = Log()



