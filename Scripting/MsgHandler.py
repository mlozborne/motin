"""
This module contains
    MsgQuPump
    MsgSocket, MsgServerThread, ClientHandlerThread
"""
from socket import socket
from time import sleep
from MessageTranslationLibrary import makeMsgStr, splitMsgStr
from Log import printLog, openLog, closeLog
from threading import Thread
import sys
import queue
import multiprocessing
import queue

class MsgOutQuPump(Thread):
    """
    Removes messages from a message queue and sends them to the controller or
    railroad via a MsgSocket
    Usage
        from MsgHandler import MsgOutQuPump
        pump = MsgOutQuPump(nm = <string>, sock = <MsgSocket>, qu = <queue.Queue or multiprocessing.Queue>)
        pump.start()
    """
    def __init__(self, nm = "1", sock = None, qu = None):
        assert(isinstance(nm, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(qu, queue.Queue) or isinstance(qu, multiprocessing.queues.Queue))
        printLog("Creating MsgOutQueuePump {0}".format(nm))
        Thread.__init__(self)
        self.nm = nm
        self.sk = sock
        self.qu = qu

    def run(self):
        printLog("Starting MsgOutQueuePump {0}".format(self.nm))
        while True:
            msg = self.qu.get()
            self.sk.send(msg)

################################################################################

class MsgInQuPump(Thread):
    """
    Reads messages from the controller or railroad via a MsgSocket and puts
    them in a queue

    Usage
        from MsgHandler import MsgInQuPump
        pump = MsgInQuPump(nm = <string>, sock = <MsgSocket>, qu = <queue.Queue or multiprocessing.Queue>)
        pump.start()
    """
    def __init__(self, nm = "1", sock = None, qu = None):
        assert(isinstance(nm, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(qu, queue.Queue) or isinstance(qu, multiprocessing.queues.Queue))
        printLog("Creating MsgInQueuePump {0}".format(nm))
        Thread.__init__(self)
        self.nm = nm
        self.sk = sock
        self.qu = qu

    def run(self):
        printLog("Starting MsgInQueuePump {0}".format(self.nm))
        while True:
            st = self.sk.receive()
            self.qu.put(makeMsgStr(st))

################################################################################

class MsgSocket(object):
    """
    This class is a wrapper for the standard socket class.
    o  A socket object can be shared between threads in process but not by
       different processes
    o  Data is restricted to train messages as defined in the MessageTranslationType
       module.
    o  Train messages are automatically converted to ascii strings before being
       sent, and these ascii strings are automatically converted back to train messages on receipt. The
       ascii strings include a two byte prefix that indicates the length of the
       rest of the string.
    o  Usage pattern on the client side.
           msk = MessageSocket()
           msk.connect(serverSideHost, serverSidePort) # Tries for 10 seconds before throwing an exception
           ...
           msk.send(msg)
           ...
           msg = msk.receive()
           ...
           msk.close()
    o  Usage pattern on the server side
           define a clientHandlerFunction for use by the client handler
              this function should close its socket when done
           msk = MessageSocket()
           msk.createMsgServerThread(serverSideHost, serverSidePort, clientHandlerFunction)


    """
    def __init__(self, nm = "1"):
        assert(isinstance(nm, str))
        printLog("msgSocket {0} created".format(nm))
        self.nm = nm

#    def createMsgServerThread(self, host, port, clientHandlerFunction):
#        printLog("Starting a server at ({0}, {1})".format(host, port))
#        MsgServerThread(host, port, clientHandlerFunction).start()

    def connect(self, host, port):
        assert(isinstance(host, str))
        assert(port > 0)
        printLog("Client socket {0} is trying to connect to ({1}, {2})".format(self.nm, host, port))
        self.inBuffer = []
        self.sock = socket()
        for i in range(10):
            if not self.sock.connect_ex((host, port)):
                printLog("Client socket {0} has connected to {1}".format(self.nm, self.sock.getpeername()))
                return
            sleep(1)
        st = "EXCEPTION: Client socket {0} FAILED to connected to ({1}, {2})".format(self.nm, host, port)
        printLog(st)
        print(st); sys.stdout.flush()
        raise

    def attach(self, sock):
        assert(isinstance(sock, socket))
        printLog("msgSocket {0} has attached standard socket {1}".format(self.nm, sock.getpeername()))
        self.sock = sock
        self.inBuffer = []

    def send(self, msg):
        assert(isinstance(msg, tuple))
        st = makeMsgStr(msg)
        self.sock.sendall(st)
        printLog("<<< Sent    {0}    to {1}".format(msg, self.sock.getpeername()))

    def close(self):
        printLog("msgSocket {0} closing ".format(self.nm))
        self.sock.close()

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strSize = self.inBuffer[0] + 128 * self.inBuffer[1]
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        printLog("    >>> Received    {0}    from {1}".format(msg, self.sock.getpeername()))
        return msg


class MsgServerThread(Thread):
    def __init__(self, nm = "1", host = None, port = None, clientHandlerFunction = None):
        assert(isinstance(nm, str))
        assert(isinstance(host, str))
        assert(isinstance(port, int))
        assert(clientHandlerFunction != None)
        printLog("Message server {0} created at ({1}, {2})".format(nm, host, port))
        Thread.__init__(self)
        self.nm = nm
        self.host = host
        self.port = port
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        printLog("Message server {0} running".format(self.nm))
        self.serverSocket = socket()
        self.serverSocket.bind((self.host, self.port))           # bind
        self.serverSocket.listen(5)                              # listen
        printLog("Message server {0} now listening at ({1}, {2})".format(self.nm, self.host, self.port))
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            printLog("Message server {0} just created a connection to client at {1}". \
                      format(self.nm, socketToClient.getpeername()))
            ClientHandlerThread("1", socketToClient, self.clientHandlerFunction).start()

class ClientHandlerThread(Thread):
    def __init__(self, nm, socketToClient, clientHandlerFunction):
        assert(isinstance(nm, str))
        assert(isinstance(socketToClient, socket))
        assert(clientHandlerFunction != None)
        printLog("Client handler {0} created for {1}".format(nm, socketToClient.getpeername()))
        Thread.__init__(self)
        self.nm = nm
        self.socketToClient = socketToClient
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        msgSocketToClient = MsgSocket()
        msgSocketToClient.attach(self.socketToClient)
        printLog("Client handler {0} running and calling {1}".format(self.nm, self.clientHandlerFunction))
        self.clientHandlerFunction(msgSocketToClient)

