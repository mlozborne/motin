############################################################################
###################### Unit Testing  for Log    ############################
############################################################################
"""
        Expected output:
            Interleaved output from multiple threads
            Each thread logs
                (a) when it is starting
                (b) multiple lines of output
                (c) when it has ended
            There should be NO BLANK LINES in the log and
            each line should contain output from a single thread.
            The line '....MIDPOINT...' appears half way through.
            In the first half lines are about 90 characters long.
            In the second half lines are twice as long.
"""

from unittest import TestCase
import threading
from Log import gLog
from time import sleep


class PrintManyLinesToLog(threading.Thread):
    def __init__(self, character):
        threading.Thread.__init__(self)
        self.character = character
        self.lineCount = 0

    def run(self):
        gLog.print("Starting " + self.character)
        tempStr = ""
        for i in range(80):
            tempStr = tempStr + self.character
        for i in range(50):
            self.lineCount += 1
            gLog.print(tempStr + ": count = " + str(self.lineCount))
        gLog.print("Ending " + self.character)


class TestLogClass(TestCase):
    def test_assertions(self):
        gLog.open('1')
        self.assertRaises(AssertionError, gLog.print, 3)          # trying to print a non string
        gLog.close()

        self.assertRaises(AssertionError, gLog.open, 1, 40)       # name is not a string
        self.assertRaises(AssertionError, gLog.open, 'a', 'cat')  # flushFrequency is not an integer
        self.assertRaises(AssertionError, gLog.open, 'a', 2.5)    # flushFrequency is not an integer
        self.assertRaises(AssertionError, gLog.open, 'a', -2)     # flushFrequency is < 0

    def test_log(self):
        gLog.open('2', 10)
        gLog.print(
            """
                    Expected output:
                        Interleaved output from multiple threads
                        Each thread logs
                            (a) when it is starting
                            (b) multiple lines of output
                            (c) when it has ended
                        There should be NO BLANK LINES in the log and
                        each line should contain output from a single thread.
                        The line '....MIDPOINT...' appears half way through.
                        In the first half lines are about 90 characters long.
                        In the second half lines are twice as long.
            """
        )
        for digit in range(10):
            PrintManyLinesToLog(str(digit)).start()
        sleep(2)
        gLog.flush()
        gLog.print('...................... MIDPOINT.........................')
        for digit in range(10):
            PrintManyLinesToLog(str(digit) + str(digit) + str(digit)).start()
        sleep(5)
        gLog.close()
        raise Exception ('EXCEPTION EXPECTED: Check correctness of output in file log_test.txt')