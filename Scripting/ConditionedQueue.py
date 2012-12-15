from threading import Condition
import math

class ConditionedQueue(object):
    def __init__(self):
        self.qu = []
        self.length = 0
        self.quCondition = Condition()

    def put(self, item):
        self.quCondition.acquire()
        self.qu.append(item)
        self.length += 1
        self.quCondition.notify()
        self.quCondition.release()

    def get(self):
        x = 0
        self.quCondition.acquire()
        while self.length == 0:
            self.quCondition.wait()
        item = self.qu.pop(0)
        self.length -= 1
        return item
        self.quCondition.notify()
        self.quCondition.release()

    def length(self):
        return self.length

############################################################################
###################### Unit Testing             ############################
############################################################################

from threading import Thread
import random
import time

class Producer(Thread):
    def __init__(self, prefix, qu):
        Thread.__init__(self)
        self.qu = qu
        self.count = 0
        self.prefix = prefix

    def run(self):
        random.seed()
        for i in range(100):
            #time.sleep(1)
#            time.sleep((0.01 + random.random())/100.0)
            self.count += 1
            self.qu.put(str(self.prefix) + str(self.count))

class Consumer(Thread):
    def __init__(self, qu):
        Thread.__init__(self)
        self.qu = qu

    def run(self):
        random.seed()
        while True:
            #time.sleep(1)
#            time.sleep((0.01 + random.random())/100.0)
            item = self.qu.get()
            print item

if __name__ == "__main__":
#    for i in range(5):
#        print chr(ord("a")+i)
    qu = ConditionedQueue()
    for i in range(10):
        Consumer(qu).start()
    for i in range(5):
        Producer(chr(ord("a")+i), qu).start()
    raw_input("press enter to quit")




