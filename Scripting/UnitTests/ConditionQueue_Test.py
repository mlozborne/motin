############################################################################
###################### Unit Testing             ############################
############################################################################

from threading import Thread
from ConditionedQueue import *

class Producer(Thread):
    def __init__(self, prefix, qu):
        Thread.__init__(self)
        self.qu = qu
        self.count = 0
        self.prefix = prefix

    def run(self):
        for i in range(100):
            self.count += 1
            self.qu.put(str(self.prefix) + str(self.count))

class Consumer(Thread):
    def __init__(self, qu):
        Thread.__init__(self)
        self.qu = qu

    def run(self):
        while True:
            item = self.qu.get()
            print item

if __name__ == "__main__":
    qu = ConditionedQueue()
    for i in range(10):
        Consumer(qu).start()
    for i in range(5):
        Producer(chr(ord("a")+i), qu).start()
    raw_input("press enter to quit")