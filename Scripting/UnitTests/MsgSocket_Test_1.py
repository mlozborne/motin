from MsgSocket import MsgSocket
#from threading import Thread
from multiprocessing import Process

from MessageTranslationTypes import SwReqMsg, kThrown, LocoSpdMsg
import sys
from Log import openLog, closeLog
from time import sleep

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
        sk = MsgSocket()
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
        sk = MsgSocket()
        sk.createMsgServerThread(self.host, self.port, clientHandlerFunction)

if __name__ == "__main__":
    openLog("main", 1)
    Client('localhost', 1234).start()
#    Server('localhost', 1234, clientHandlerFunction).start()
    print("press enter to close the log")
    closeLog()


