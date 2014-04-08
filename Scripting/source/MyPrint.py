from threading import Condition

printCondition = Condition()


def myPrint(st):
    global printCondition
    printCondition.acquire()
    print(st)
    printCondition.notify()
    printCondition.release()


