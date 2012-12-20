class Producer(Thread):
    def __init__(self, qu, id):
        self.qu = qu
        self.id = id

    def run(self):
        for i in range(50):
            self.qu.put(str(id)+str(i))

class Consumer(Thread):
    def __init__(self, qu, id):
        self.qu = qu
        self.id = id

    def run(self):
        item = self.qu.get()

def main():
