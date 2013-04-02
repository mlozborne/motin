"""
This file contains several related parts.

1) MsgSocket
   -- lowest level
   -- wrapper for sockets
   -- connect, send, receive, attach, close
   -- test with MsgSocket_Test.py
2) MsgServerThread and ClientHandlerThread
   -- sets up a listener and spawns handlers for the clients
   -- test with MsgHandler_Test.py test 1
3) MsgOutQuPump
   -- a thread
3) MsgInQuPump, MsgOutQuPump, MsgInternalQuPump

"""

from multiprocessing.queues import Queue
from socket import socket
from time import sleep
from MessageTranslationLibrary import makeMsgStr, splitMsgStr
from MessageTranslationTypes import *
from Log import printLog
from threading import Thread
import sys
import multiprocessing
from collections import namedtuple

#InQuListEntry  = namedtuple('InQuListEntry', 'qu, msgTypes')
InQuListEntry       = namedtuple('InQuListEntry', 'inQu, interests')
AddInterestMsg      = namedtuple('AddInterestMsg', 'inQuNum, interest')
RemoveInterestMsg   = namedtuple('RemoveInterestMsg', 'inQuNum, interest')
RemoveAllInterestsMsg = namedtuple('RemoveAllInterestsMsg', 'inQuNum')
InQuListMsgs = (AddInterestMsg, RemoveInterestMsg, RemoveAllInterestsMsg)

# Where
#    inQuNum               0..<number of inQu's - 1>
#    inQu                  multiprocessing.queueus.Queue
#    interest              a message type, e.g. PutInitOutcomeMsg
#    interests             a list of message types

################################################################################

class MsgHandler(object):
    def __init__(self, name = None, inQu = None, inQuNum = None, outQu = None):
        printLog("MsgHandler {0}: initializing".format(name))
        assert(isinstance(name, str))
        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        assert(0 <= inQuNum)
        self.name = name
        self.inQu = inQu
        self.inQuNum = inQuNum
        self.outQu = outQu

    def addInterest(self, msgType):
        printLog("MsgHandler {0}: adding interest {1}".format(self.name, msgType))
        assert(msgType in ControllerInMsgs)
        self.outQu.put(AddInterestMsg(inQuNum = self.inQuNum, interest = msgType))
        #printLog("MsgHandler {0}: finished adding interest {1}".format(self.name, msgType))

    def removeInterest(self, msgType):
        printLog("MsgHandler {0}: removing interest {1}".format(self.name, msgType))
        assert(msgType in ControllerInMsgs)
        self.outQu.put(RemoveInterestMsg(inQuNum = self.inQuNum, interest = msgType))

    def close(self):
        printLog("MsgHandler {0}: removing all interests".format(self.name))
        self.outQu.put(RemoveAllInterestsMsg(inQuNum = self.inQuNum))

    def getBlocking(self):
        msg = self.inQu.get()
        #printLog("MsgHandler {0}: getBlocking message {1}".format(self.name, msg))
        return msg

    def getNonblocking(self):
        try:
            msg = self.inQu.get(False) # non-blocking
            #printLog("MsgHandler {0}: getNonblocking message {1}".format(self.name, msg))
            return msg
        except multiprocessing.queues.Empty:
            printLog("MsgHandler {0}: getNonblocking empty qu exception)".format(self.name))
            raise multiprocessing.queues.Empty

    def put(self, msg):
        #printLog("MsgHandler {0}: putting message {1}".format(self.name, msg))
        self.outQu.put(msg)

################################################################################
InQuListEntry       = namedtuple('InQuListEntry', 'inQu, interests')

class MsgPumpHandler(object):
    def __init__(self, name = None, host = None, port = None, inQuList = None, outQu = None):
        printLog("MsgPumpHandler {0}: initializing".format(name))
        assert(isinstance(name, str))
        assert(isinstance(host, str))
        assert(port > 1000)
        assert(isinstance(inQuList, list))
        for x in inQuList:
            assert(isinstance(x, InQuListEntry))
            assert(isinstance(x.inQu, multiprocessing.queues.Queue))
            assert(isinstance(x.interests, list))
            #print(str(x))
            for y in x.interests:
                assert(y in ControllerInMsgs)
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        self.name = name
        self.host = host
        self.port = port
        self.inQuList = inQuList
        self.outQu = outQu

    def startPumps(self):
        #printLog("MsgHandler {0}: trying to start pumps".format(self.name))
        internalQu = Queue()
        msgSock = MsgSocket(name = self.name)
        msgSock.connect(self.host, self.port)
        MsgInternalQuPump(name = self.name, internalQu = internalQu, inQuList = self.inQuList).start()
        MsgInQuPump(name = self.name, sock = msgSock, inQuList = self.inQuList).start()
        MsgOutQuPump(name = self.name, sock = msgSock, outQu = self.outQu, internalQu = internalQu).start()
        printLog("MsgHandler {0}: has started pumps".format(self.name))

################################################################################

class MsgInternalQuPump(Thread):
    def __init__(self, name = None, internalQu = None, inQuList = None):
        Thread.__init__(self)
        assert(isinstance(name, str))
        assert(isinstance(internalQu, multiprocessing.queues.Queue))
        assert(isinstance(inQuList, list))
        for x in inQuList:
            assert(isinstance(x, InQuListEntry))
            assert(isinstance(x.inQu, multiprocessing.queues.Queue))
            assert(isinstance(x.interests, list))
            #print(str(x.interests))
            for y in x.interests:
                #print("..." + str(y))
                assert(y in ControllerInMsgs)
        printLog("MsgInternalQuPump {0}: initializing".format(name))
        self.name = name
        self.internalQu = internalQu
        self.inQuList = inQuList

    def run(self):           
        printLog("MsgInternalQuPump {0}: starting".format(self.name))
        while True:
            msg = self.internalQu.get()  
            if isinstance(msg, RemoveAllInterestsMsg):
                printLog("MsgInternalQuPump {0}: removing all interests for inQuNum {1}".format(self.name, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # else remove all interests for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    self.inQuList[msg.inQuNum].interests = []
            elif isinstance(msg, AddInterestMsg):
                printLog("MsgInternalQuPump {0}: adding interest {1} for inQuNum {2}".format(self.name, msg.interest, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # elif the inQuNum already has this interest raise an exception
                # else add an interest for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    x = self.inQuList[msg.inQuNum]
                    if msg.interest in x.interests:
                        pass    # The interest could have been have added at a higher level
                        #raise Exception("MsgInternalQuPump {0}: Can't add. inQuNum {1} already has {2}".format(self.name, msg.inQuNum, msg.interest))
                    else:
                        x.interests.append(msg.interest)
            elif isinstance(msg, RemoveInterestMsg):
                printLog("MsgInternalQuPump {0}: removing interest {1} for inQuNum {2}".format(self.name, msg.interest, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # elif there is no matching interest raise an exception
                # else remove an interest for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    x = self.inQuList[msg.inQuNum]
                    if not msg.interest in x.interests:
                        raise Exception("MsgInternalQuPump {0}: Can't remove. inQuNum {1} doesn't have {2}".format(self.name, msg.name, msg.interest))
                    else:
                        x.interests.remove(msg.interest)

################################################################################

class MsgOutQuPump(Thread):
    def __init__(self, name = None, sock = None, outQu = None, internalQu = None):
        Thread.__init__(self)
        assert(isinstance(name, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        assert(isinstance(internalQu, multiprocessing.queues.Queue))
        printLog("MsgOutQuPump {0}: initializing".format(name))
        self.name = name
        self.sk = sock
        self.outQu = outQu
        self.internalQu = internalQu

    def run(self):
        printLog("MsgOutQuPump {0}: starting".format(self.name))
        while True:
            msg = self.outQu.get()
            if type(msg) in InQuListMsgs:
                #printLog("MsgOutQuPump {0}: getting ready to put {1}".format(self.name, msg))
                self.internalQu.put(msg)
            else:
                #printLog("MsgOutQuPump {0}: getting ready to send {1}".format(self.name, msg))
                self.sk.send(msg)

################################################################################

class MsgInQuPump(Thread):
    def __init__(self, name = None, sock = None, inQuList = None):
        Thread.__init__(self)
        assert(isinstance(name, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(inQuList, list))
        for x in inQuList:
            assert(isinstance(x, InQuListEntry))
            assert(isinstance(x.inQu, multiprocessing.queues.Queue))
            assert(isinstance(x.interests, list))
            for y in x.interests:
                assert(y in ControllerInMsgs)
        printLog("MsgInQuPump {0}: initializing".format(name))
        self.name = name
        self.sk = sock
        self.inQuList = inQuList

    def run(self):
        printLog("MsgInQuPump {0}: starting".format(self.name))
        while True:
            st = self.sk.receive()
            msg = makeMsgStr(st)
            for x in self.inQuList:
                q = x.inQu
                messageTypes = x.interests
                if type(msg) in messageTypes:
                    q.put(msg)

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
    def __init__(self, name = "1"):
        assert(isinstance(name, str))
        printLog("MsgSocket {0}: initializing".format(name))
        self.name = name

    def connect(self, host, port):
        assert(isinstance(host, str))
        assert(port > 0)
        printLog("MsgSocket {0}: is trying to connect to ({1}, {2})".format(self.name, host, port))
        self.inBuffer = []
        self.sock = socket()
        for i in range(10):
            if not self.sock.connect_ex((host, port)):
                printLog("MsgSocket {0}: has connected to {1}".format(self.name, self.sock.getpeername()))
                return
            sleep(1)
        st = "MsgSocket {0}: EXCEPTION: Socket FAILED to connected to ({1}, {2})".format(self.name, host, port)
        printLog(st)
        print(st); sys.stdout.flush()
        raise

    def attach(self, sock):
        assert(isinstance(sock, socket))
        printLog("MsgSocket {0}: has attached standard socket {1}".format(self.name, sock.getpeername()))
        self.sock = sock
        self.inBuffer = []

    def send(self, msg):
        assert(isinstance(msg, tuple))
        st = makeMsgStr(msg)
        self.sock.sendall(st)
        printLog("<<< Sent    {0}    to {1}".format(msg, self.sock.getpeername()))

    def close(self):
        printLog("MsgSocket {0}: closing ".format(self.name))
        self.sock.close()

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strSize = self.inBuffer[0] + 128 * self.inBuffer[1]
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)         # WARNING: if length of buf is 0, then the connection has been broken
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        printLog("    >>> Received    {0}    from {1}".format(msg, self.sock.getpeername()))
        return msg


class MsgServerThread(Thread):
    def __init__(self, name = "1", host = None, port = None, clientHandlerFunction = None):
        assert(host == None or isinstance(name, str))
        assert(isinstance(host, str))
        assert(isinstance(port, int))
        assert(clientHandlerFunction != None)
        printLog("Message server {0}: initializing at ({1}, {2})".format(name, host, port))
        Thread.__init__(self)
        self.name = name
        if host == None:
            host = socket.gethostname()
        self.host = host
        self.port = port
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        printLog("Message server {0}: running".format(self.name))
        self.serverSocket = socket()
        self.serverSocket.bind((self.host, self.port))           # bind
        self.serverSocket.listen(5)                              # listen
        printLog("Message server {0}: now listening at ({1}, {2})".format(self.name, self.host, self.port))
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            printLog("Message server {0}: just created a connection to client at {1}". \
                      format(self.name, socketToClient.getpeername()))
            ClientHandlerThread("1", socketToClient, self.clientHandlerFunction).start()

class ClientHandlerThread(Thread):
    def __init__(self, name, socketToClient, clientHandlerFunction):
        assert(isinstance(name, str))
        assert(isinstance(socketToClient, socket))
        assert(clientHandlerFunction != None)
        printLog("Client handler {0}: created for {1}".format(name, socketToClient.getpeername()))
        Thread.__init__(self)
        self.name = name
        self.socketToClient = socketToClient
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        msgSocketToClient = MsgSocket()
        msgSocketToClient.attach(self.socketToClient)
        printLog("Client handler {0}: running and calling {1}".format(self.name, self.clientHandlerFunction))
        self.clientHandlerFunction(msgSocketToClient)

