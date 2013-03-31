############################################################################
###################### Unit Testing             ############################
############################################################################

import threading
from MyPrint import myPrint

class ManyLinesToScreen(threading.Thread):
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
    for i in range(9):
        ManyLinesToScreen(str(i)).start()
    raw_input("press enter to quit")
