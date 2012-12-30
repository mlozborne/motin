"""
Test1: MsgSocket
    o There are three processes and each process has its own log
        process main with log_main.txt
        process main creates and starts a Client process and a Server process
        process Client with log_client.txt
        process Server with log_server.txt
    o Process Server listens for connect requests at port 1234 and
      then spawns threads to service  the clients.
    o Process Client connects to the server.
    o The client and the service exchange messages.
    o The service is defined by the function "clientHandlerFunction"
Test2: MsgQuPump
    open a log
    start the railroad
    create a qu
    create a socket to ('locohost', 1234)
    create a pump
    add to the qu messages to open and close some switches
    close the log
    pause
    end the program
"""
from MsgHandler import MsgSocket, MsgInQuPump, MsgOutQuPump, MsgServerThread, waitFor

from multiprocessing import Process, Queue
from MessageTranslationTypes import *
import sys
from Log import openLog, closeLog, printLog
from time import sleep
import StartAndKill as sak
from threading import Thread

def clientHandlerFunction(socketToClient):
    sk = socketToClient
    for i in range(5):
        msg = LocoSpdMsg(slot = 11, speed = 100)
        sk.send(msg)
        print("Handler sent message {0}".format(msg)); sys.stdout.flush()
        msg = sk.receive()
        print("Handler received message {0}".format(msg)); sys.stdout.flush()
    socketToClient.close()

class Client(Process):
    def __init__(self, host, port):
        Process.__init__(self)
        self.host = host
        self.port = port

    def run(self):
        openLog("Client", 1)
        sk = MsgSocket("1")
        sk.connect(self.host, self.port)
        for i in range(5):
            msg = SwReqMsg(switch = 11, direction = kThrown)
            sk.send(msg)
            print("Client sent message {0}".format(msg)); sys.stdout.flush()
            msg = sk.receive()
            print("Client received message {0}".format(msg)); sys.stdout.flush()
        sk.close()
        closeLog()

class Server(Process):
    def __init__(self, host, port, clientHandlerFunction):
        Process.__init__(self)
        self.host = host
        self.port = port
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        openLog("Server", 1)
        MsgServerThread("1", self.host, self.port, clientHandlerFunction).start()

class Consumer(Thread):
    def __init__(self, name, inQu):
        printLog("Consumer {0} created".format(name))
        Thread.__init__(self)
        self.inQu = inQu
        self.name = name

    def run(self):
        printLog("Consumer {0} running".format(self.name))
        while True:
            msg = self.inQu.get()
            printLog("Consumer {0} received {1}".format(self.name, msg))

class Producer(Thread):
    def __init__(self, name, outQu):
        printLog("Producer {0} created".format(name))
        Thread.__init__(self)
        self.outQu = outQu
        self.name = name

    def run(self):
        printLog("Producer {0} running".format(self.name))
        for i in range(1,11):
            self.outQu.put(SwReqMsg(switch = i, direction = kThrown))

if __name__ == "__main__":

    ###################################################
    # Test 1
    
#    Client('localhost', 1200).start()
#    Server('localhost', 1200, clientHandlerFunction).start()

    ###################################################
    # Test 2

    openLog("main")                                           #open log
    sak.start("simulator")                                    #start simulator
    sk = MsgSocket(name = "1")                                  #create msgSocket
    sk.connect("localHost", 1234)                             #connect socket to simulator
    sleep(3)
    
    inQuList = []
    for i in range(3):
        inQuList.append(Queue())
    MsgInQuPump(name = "in pump", sock = sk, inQuList = inQuList).start()

    outQu = Queue()
    MsgOutQuPump(name = "out pump", sock = sk, qu = outQu).start()

    Producer("1", outQu).start()
    for i in range(3):
        Consumer(str(i+1), inQuList[i]).start()

    input("press enter to quit")
    sak.kill("simulator")                                     #kill simulator
    closeLog()

    ###################################################
    # Test 3
    
#    openLog()
#
#    qu = Queue()
#
#    for i in range(1,6):
#        qu.put(SwRepMsg(switch = i, direction = kClosed))
#
#    qu.put(PutReadLayoutResponseMsg(responseFlag=100, code=200))
#
#    for i in range(1,6):
#        qu.put(SwRepMsg(switch = i, direction = kThrown))
#
#    qu.put(PutInitOutcomeMsg(physAdd = 1111, physSlot = 1, virtAdd = 11, virtSlot = 5))
#
#    for i in range(1,6):
#        qu.put(SwRepMsg(switch = i, direction = kClosed))
#
#    msg = waitFor(qu, PutReadLayoutResponseMsg(responseFlag=100, code=200))
#    print(str(msg))
#
#    msg = waitFor(qu, PutInitOutcomeMsg(physAdd = 1111, physSlot = 125, virtAdd = 0, virtSlot = 0))
#    print(str(msg))
#
#    print("qu contains {0} messages".format(qu.qsize()))





