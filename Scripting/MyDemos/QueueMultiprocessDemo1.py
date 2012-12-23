from multiprocessing import Process, Queue
import sys

class Producer(Process):
    def __init__(self, qu, nm):
        Process.__init__(self)
        print("creating {0}".format(self.name)); sys.stdout.flush()
        self.qu = qu
        self.nm = nm

    def run(self):
        print("running producer {0}".format(self.nm)); sys.stdout.flush()
        for i in range(50):
            self.qu.put(str(self.nm) + str(i))

class Consumer(Process):
    def __init__(self, qu, nm):
        Process.__init__(self)
        print("creating {0}".format(self.name)); sys.stdout.flush()
        self.qu = qu
        self.nm = nm

    def run(self):
        print("running consumer {0}".format(self.nm)); sys.stdout.flush()
        while True:
            item = self.qu.get()
            print(self.nm + " " + item); sys.stdout.flush()

if __name__ == "__main__":
    qu = Queue(5)            # This queue is shared by all producer and consumer processes

    numConsumers = 2
    numProducers = 3
    consumers = [Consumer(qu, chr(ord('A') + i)) for i in range(numConsumers)]
    producers = [Producer(qu, chr(ord('a') + i)) for i in range(numProducers)]

    for c in consumers:
        c.start()
    
    for p in producers:
        p.start()

    print("The end!!! qu size = {0}".format(qu.qsize())); sys.stdout.flush()




