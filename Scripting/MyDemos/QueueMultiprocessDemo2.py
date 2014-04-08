from multiprocessing import Process, JoinableQueue, Queue, cpu_count
import time
import sys


class Consumer(Process):

    def __init__(self, task_queue, result_queue):
        Process.__init__(self)
        print("creating {0}".format(self.name)); sys.stdout.flush()
        self.task_queue = task_queue
        self.result_queue = result_queue

    def run(self):
        print("{0}: Running".format(self.name)); sys.stdout.flush()
        proc_name = self.name
        while True:
            next_task = self.task_queue.get()
            if next_task is None:
                # Poison pill means shutdown
                print('%s: Exiting' % proc_name); sys.stdout.flush()
                self.task_queue.task_done()
                break
            print('%s: %s' % (proc_name, next_task)); sys.stdout.flush()
            answer = next_task()
            self.task_queue.task_done()
            self.result_queue.put(answer)


class Task(object):
    def __init__(self, a, b):
        self.a = a
        self.b = b

    def __call__(self):
        time.sleep(0.5) # pretend to take some time to do the work
        return '%s * %s = %s' % (self.a, self.b, self.a * self.b)

    def __str__(self):
        return '%s * %s' % (self.a, self.b)


if __name__ == '__main__':
    # Establish communication queues
    tasks = JoinableQueue()
    results = Queue()

    # Start consumers
    num_consumers = cpu_count() * 2
    print('Creating %d consumers' % num_consumers); sys.stdout.flush()
    consumers = [Consumer(tasks, results) for i in range(num_consumers)]
    for w in consumers:
        w.start()

    # Enqueue jobs
    num_jobs = 10
    for i in range(num_jobs):
        tasks.put(Task(i, i))

    # Add a poison pill for each consumer
    for i in range(num_consumers):
        tasks.put(None)

    # Wait for all of the tasks to finish
    tasks.join()

    # Start printing results
    for i in range(num_jobs):
        result = results.get()
        print('Result:', result); sys.stdout.flush()
