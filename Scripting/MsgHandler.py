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
InQuListEntry       = namedtuple('InQuListEntry', 'owner, inQu, interests')
AddInQuMsg          = namedtuple('AddQuMsg', 'owner, qu')
RemoveInQuMsg       = namedtuple('RemoveQuMsg', 'owner')
AddInterestMsg      = namedtuple('AddInterestMsg', 'owner, interest')
RemoveInterestMsg   = namedtuple('RemoveInterestMsg', 'owner, interest')
InQuListMsgs = (AddInQuMsg, RemoveInQuMsg, AddInterestMsg, RemoveInterestMsg)

# Where
#    owner                 str name of the process requesting an inQu
#    inQu                  multiprocessing.queueus.Queue
#    interest              a message type, e.g. PutInitOutcomeMsg
#    interests             a list of message types

################################################################################

def waitFor(qu, msg):
    printLog("waitFor: msg = {0}".format(msg))
    assert(isinstance(qu, multiprocessing.queues.Queue))
    assert(isinstance(msg, tuple))
    while True:
        while True:
            m = qu.get()
            if type(m) == type(msg):
                break
        if isinstance(m, PutReadLayoutResponseMsg):
            break
        if isinstance(m, InputRepMsg) and m.sensor == msg.sensor:
            break
        if isinstance(m, PutInitOutcomeMsg) and m.physAdd == msg.physAdd:
            break
        if isinstance(m, PutSensorStateMsg) and m == msg:
            break
    return m

################################################################################

class MsgPumpHandler(object):
    def __init__(self, name = None, host = None, port = None, outQu = None):
        assert(isinstance(name, str))
        assert(isinstance(host, str))
        assert(port > 1000)
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        self.name = name
        self.host = host
        self.port = port
        self.outQu = outQu

    def startPump(self):
        inQuList = []
        internalQu = Queue()
        msgSock = MsgSocket(name = self.name)
        msgSock.connect(self.host, self.port)
        MsgInternalQuPump(name = self.name, internalQu = internalQu, inQuList = inQuList).start()
        MsgInQuPump(name = self.name, sock = msgSock, inQuList = inQuList).start()
        MsgOutQuPump(name = self.name, sock = msgSock, outQu = self.outQu, internalQu = internalQu).start()

################################################################################

class MsgInternalQuPump(Thread):
    def __init__(self, name = "1", internalQu = None, inQuList = None):
        assert(isinstance(name, str))
        assert(isinstance(internalQu, multiprocessing.queues.Queue))
        assert(isinstance(inQuList, list))
        for x in inQuList:
            assert(isinstance(x.owner, str))
            assert(isinstance(x.inQu, multiprocessing.queues.Queue))
            assert(isinstance(x.interests, list))
            for y in x.interests:
                assert((y in InQuListMsgs) or (y in ControllerOutMsgs))
        printLog("MsgOutQueuePump {0}: initializing".format(name))
        Thread.__init__(self)
        self.name = name
        self.internalQu = internalQu
        self.inQuList = inQuList

    def run(self):
        def getListEntryForOwner(ow):
            for x in self.inQuList:
                if x.owner == ow:
                    return x
            return None
            
        def interestInListEntry(entry, int):
            return int in entry.interests
            
        printLog("MsgOutQueuePump {0}: starting".format(self.name))
        while True:
            msg = self.internalQu.get()  
            if isinstance(msg, AddInQuMsg):
                # if there is already a qu for this owner raise an exception
                # else add a inQuListEntry for this owner
                if getListEntryForOwner(msg.owner) == None:
                    self.inQuList.append(InQuListEntry(owner = msg.owner, qu = msg.qu))
                else:
                    raise Exception("MsgInternalQuPump: Can't add inQu. {0} already has one.".format(msg.owner))
            elif isinstance(msg, RemoveInQuMsg):
                # if there is no entry for this owner raise an exception
                # else remove an inQuListEntry for this owner
                x = getListEntryForOwner(msg.owner)
                if x == None:
                    raise Exception("MsgInternalQuPump: Can't remove inQu. {0} doesn't have one.".format(msg.owner))
                else:
                    self.inQuList.remove(x)
            elif isinstance(msg, AddInterestMsg):
                # if there is no entry for this owner raise an exception
                # elif the owner already has this interest raise an exception
                # else add an interest for this owner
                x = getListEntryForOwner(msg.owner)
                if x == None:
                    raise Exception("MsgInternalQuPump: Can't add interest. {0} doesn't have an inQu".format(msg.owner))
                else:
                    if interestInListEntry(x, msg.interest):
                        raise Exception("MsgInternalQuPump: Can't add interest. {0} already has {1}".format(msg.owner, msg.interest))
                    else:
                        x.interests.append(msg.interest)
            elif isinstance(msg, RemoveInterestMsg):
                # if there is no entry for this owner raise an exception
                # elif there is no matching interest raise an exception
                # else remove an interest for this owner
                x = getListEntryForOwner(msg.owner)
                if x == None:
                    raise Exception("MsgInternalQuPump: Can't remove interest. {0} doesn't have an inQu".format(msg.owner))
                else:
                    if not interestInListEntry(x, msg.interest):
                        raise Exception("MsgInternalQuPump: Can't remove interest. {0} doesn't have {1}".format(msg.owner, msg.interest))
                    else:
                        x.interests.remove(msg.interest)

################################################################################

class MsgOutQuPump(Thread):
    def __init__(self, name = "1", sock = None, outQu = None, internalQu = None):
        assert(isinstance(name, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        assert(isinstance(internalQu, multiprocessing.queues.Queue))
        printLog("MsgOutQueuePump {0}: initializing".format(name))
        Thread.__init__(self)
        self.name = name
        self.sk = sock
        self.outQu = outQu
        self.internalQu = internalQu

    def run(self):
        printLog("MsgOutQueuePump {0}: starting".format(self.name))
        while True:
            msg = self.qu.get()
            if msg in InQuListMsgs:
                self.internalQu.put(msg)
            else:
                self.sk.send(msg)

################################################################################

class MsgInQuPump(Thread):
    def __init__(self, name = "1", sock = None, inQuList = None):
        assert(isinstance(name, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(inQuList, list))
        for x in inQuList:
            assert(isinstance(x.owner, str))
            assert(isinstance(x.inQu, multiprocessing.queues.Queue))
            assert(isinstance(x.interests, list))
            for y in x.interests:
                assert((y in InQuListMsgs) or (y in ControllerOutMsgs))
        printLog("MsgInQueuePump {0}: initializing".format(name))
        Thread.__init__(self)
        self.name = name
        self.sk = sock
        self.inQuList = inQuList

    def run(self):
        printLog("MsgInQueuePump {0}: starting".format(self.name))
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
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        printLog("    >>> Received    {0}    from {1}".format(msg, self.sock.getpeername()))
        return msg


class MsgServerThread(Thread):
    def __init__(self, name = "1", host = None, port = None, clientHandlerFunction = None):
        assert(isinstance(name, str))
        assert(isinstance(host, str))
        assert(isinstance(port, int))
        assert(clientHandlerFunction != None)
        printLog("Message server {0}: initializing at ({1}, {2})".format(name, host, port))
        Thread.__init__(self)
        self.name = name
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

