from multiprocessing import Process, Queue
import sys
import time


class Producer(Process):
    def __init__(self, qu, name):
        Process.__init__(self)
        print("creating producer {0}".format(name)); sys.stdout.flush()
        self.qu = qu
        self.name = name

    def run(self):
        print("running producer {0}".format(self.name)); sys.stdout.flush()
        for i in range(50):
            time.sleep(0.01) # pretend to take some time to do the work
            self.qu.put(str(self.name) + str(i))


class Consumer(Process):
    def __init__(self, qu, name):
        Process.__init__(self)
        print("creating consumer {0}".format(name)); sys.stdout.flush()
        self.qu = qu
        self.name = name

    def run(self):
        print("running consumer {0}".format(self.name)); sys.stdout.flush()
        while True:
            item = self.qu.get()
            # time.sleep(0.002) # pretend to take some time to do the work
            print(self.name + " " + item); sys.stdout.flush()

if __name__ == "__main__":
    queue = Queue(5)            # This queue is shared by all producer and consumer processes

    numConsumers = 5
    numProducers = 3
    consumers = [Consumer(queue, chr(ord('A') + i)) for i in range(numConsumers)]
    producers = [Producer(queue, chr(ord('a') + i)) for i in range(numProducers)]

    for c in consumers:
        c.start()
    
    for p in producers:
        p.start()

    print("The end!!! qu size = {0}".format(queue.qsize())); sys.stdout.flush()




