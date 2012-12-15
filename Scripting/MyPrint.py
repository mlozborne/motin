from threading import Condition

printCondition = Condition()

def myPrint(st):
    printCondition.acquire()
    print st
    printCondition.notify()
    printCondition.release()


