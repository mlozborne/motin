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






