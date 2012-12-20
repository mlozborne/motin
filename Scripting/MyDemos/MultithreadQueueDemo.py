from queue import Queue

from threading import Thread
class Producer(Thread):
    def __init__(self, qu, nm):
        Thread.__init__(self)
        self.qu = qu
        self.nm = nm

    def run(self):
        for i in range(50):
            self.qu.put(str(self.nm)+str(i))

class Consumer(Thread):
    def __init__(self, qu, nm):
        Thread.__init__(self)
        self.qu = qu
        self.nm = nm

    def run(self):
        while True:
            item = self.qu.get()
            print(self.nm + " " + item)

def main():
    qu = Queue(5)            # This queue is shared by all producer and consumer processes
    p1 = Producer(qu, "a")
    p2 = Producer(qu, "b")
    p3 = Producer(qu, "c")
    c1 = Consumer(qu, "C")
    c2 = Consumer(qu, "D")
    c1.start()
    c2.start()
    p1.start()
    p2.start()
    p3.start()

main()

