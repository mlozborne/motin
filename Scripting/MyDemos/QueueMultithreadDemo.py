from queue import Queue
from threading import Thread

class Producer(Thread):
    def __init__(self, qu, name):
        Thread.__init__(self)
        self.qu = qu
        self.name = name

    def run(self):
        for i in range(50):
            self.qu.put(str(self.name)+str(i))

class Consumer(Thread):
    def __init__(self, qu, name):
        Thread.__init__(self)
        self.qu = qu
        self.name = name

    def run(self):
        while True:
            item = self.qu.get()
            print(self.name + " " + item)

def main():
    qu = Queue(5)            # This queue is shared by all producer and consumer processes

    numConsumers = 2
    numProducers = 3
    consumers = [Consumer(qu, chr(ord('A')+i)) for i in range(numConsumers)]
    producers = [Producer(qu, chr(ord('a')+i)) for i in range(numProducers)]

    for c in consumers:
        c.start()
    for p in producers:
        p.start()

main()

