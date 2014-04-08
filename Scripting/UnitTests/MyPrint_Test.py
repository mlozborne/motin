############################################################################
###################### Unit Testing             ############################
############################################################################

import threading
from MyPrint import myPrint


def raw_input(st):
    return input(st)


class PrintManyLinesToScreen(threading.Thread):
    def __init__(self, st):
        threading.Thread.__init__(self)
        self.st = st
        self.count = 0

    def run(self):
        myPrint("Starting " + self.st)
        tempSt = ""
        for i in range(80):
            tempSt = tempSt + self.st
        for i in range(47):
            self.count += 1
            myPrint(tempSt + ": count = " + str(self.count))
        myPrint("Ending " + self.st)


if __name__ == "__main__":
    for ii in range(9):
        PrintManyLinesToScreen(str(ii)).start()
