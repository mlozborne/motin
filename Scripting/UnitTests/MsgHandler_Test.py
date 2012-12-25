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
from MsgHandler import MsgSocket, MsgQuPump, MsgServerThread
from multiprocessing import Process, Queue
from MessageTranslationTypes import SwReqMsg, kThrown, kClosed, LocoSpdMsg
import sys
from Log import openLog, closeLog
from time import sleep
import StartAndKill as sak
from queue import Queue

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

if __name__ == "__main__":

    ###################################################
    # Test 1
#    Client('localhost', 1200).start()
#    Server('localhost', 1200, clientHandlerFunction).start()

    ###################################################
    # Test 2
    openLog("main")                                           #open log
    sak.start("simulator")                                    #start simulator
    qu = Queue()                                              #create queue
    sk = MsgSocket(nm = "1")                                  #create msgSocket
    sk.connect("localHost", 1234)                             #connect socket to simulator
    pump = MsgQuPump(nm = "1", sock = sk, qu = qu).start()    #create a message pump for queue-->simulator
    for i in range(1,10):                                     #put messages on queue
        qu.put(SwReqMsg(switch=i, direction=kClosed))
    sleep(4)
    for i in range(1,10):
        qu.put(SwReqMsg(switch=i, direction=kThrown))
    input("press enter to quit")
    sak.kill("simulator")                                     #kill simulator
    closeLog()


