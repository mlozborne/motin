"""
This file contains several related parts.

Sockets
   MsgSocket object
       -- lowest level
       -- wrapper for sockets
       -- connect, send, receive, attach, close
       -- test with MsgSocket_Test.py
   MsgServerThread and ClientHandlerThread
       -- sets up a listener and spawns handlers for the clients
       -- test with MsgHandler_Test.py "test 1"
Pumps
   MsgOutQuPump thread
       -- moves messages from an out queue to either an internal queue or to a message socket
       -- test with MsgHandler_Test.py "test 2"
   MsgInQuPump thread
       -- moves messages from a socket to all queues that are interested
       -- test with MsgHandler_Test.py "test 2"
   MsgInternalQuPump thread
       -- get interest messages from internalQu and update interests in the inQuList
       -- test with Throttle_Test.py and GuiThrottle_Test.py
   MsgPumpHandler object
       -- creates message socket object that links to simulator/locobuffer
       -- creates internal queue
       -- creates and starts pumps and passes them the message socket and queues
       -- test with Throttle_Test.py and GuiThrottle_Test.py

User level object
   MsgHandler object
       -- each message consumer and each message producer needs exactly one of these
       -- allows consumer/producer threads to exchange messages with the railroad and/or each other
       -- test with Throttle_Test.py and GuiThrottle_Test.py
"""

from Log                       import gLog
from MessageTranslationTypes   import InputRepMsg
from MessageTranslationTypes   import PutInitOutcomeMsg
from MessageTranslationTypes   import PutReadLayoutResponseMsg
from MessageTranslationTypes   import PutSensorStateMsg
from MessageTranslationLibrary import makeMsgStr
from MessageTranslationLibrary import splitMsgStr
from MessageTranslationTypes   import PutPathMsg
from MessageTranslationTypes   import ControllerInMsgs
from MessageTranslationTypes   import PutTrainPositionMsg
from time                      import sleep

from collections import namedtuple
import multiprocessing
from multiprocessing.queues import Queue
from socket import socket
from threading import Thread

CommunicationsPackage = namedtuple('CommunicationsPackage', 'inQu, inQuNum, outQu')

InQuListEntry         = namedtuple('InQuListEntry', 'inQu, interests')
AddInterestMsg        = namedtuple('AddInterestMsg', 'inQuNum, interest')
RemoveInterestMsg     = namedtuple('RemoveInterestMsg', 'inQuNum, interest')
RemoveAllInterestsMsg = namedtuple('RemoveAllInterestsMsg', 'inQuNum')
InQuListMsgs          = (AddInterestMsg, RemoveInterestMsg, RemoveAllInterestsMsg)

# Where
#    inQuNum               0..<number of inQus - 1>
#    inQu                  multiprocessing.queues.Queue
#    interest              a message type, e.g. PutInitOutcomeMsg
#    interests             a list of message types

################################################################################


class CommunicationResources(object):
    """
    The top level program creates one of these with the numberOfPackages equal to
    the total number of MsgHandlers needed during the life time of the system.

    Each message handler must be passed a CommunicationsPackage consisting of an
       inQu, inQuNum, outQu
    obtained by calling getNextPackage

    Any process that creates one or more message handlers must be passed the same
    number of CommunicationsPackage
    """

    def __init__(self, name = None, host = None, port = None, numberOfPackages = None):
        self.name = name
        self.host = host
        self.port = port
        self.numberOfPackages = numberOfPackages
        self.packagesAllocated = 0
        self.outQu = Queue()
        self.inQuList = []
        for i in range(numberOfPackages):
            q = Queue()
            self.inQuList.append(InQuListEntry(inQu = q, interests = []))
        msgPumpHandler = MsgPumpHandler(name = self.name, host = self.host, port = self.port,
                                        inQuList = self.inQuList, outQu = self.outQu)
        msgPumpHandler.startPumps()

    def getNextPackage(self):
        if self.packagesAllocated >= self.numberOfPackages:
            raise "allocated too many resource packages"
        i = self.packagesAllocated
        self.packagesAllocated += 1
        comPkg = CommunicationsPackage(inQu = self.inQuList[i].inQu, inQuNum = i, outQu = self.outQu)
        return comPkg


class MsgHandler(object):
    def __init__(self, name = None, comPkg = None):
        gLog.print("MsgHandler {0}: initializing".format(name))
        assert(isinstance(name, str))

        assert(isinstance(comPkg, CommunicationsPackage))
        inQu = comPkg.inQu
        inQuNum = comPkg.inQuNum
        outQu = comPkg.outQu

        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        assert(0 <= inQuNum)
        self.name = name
        self.inQu = inQu
        self.inQuNum = inQuNum
        self.outQu = outQu

    def addInterest(self, msgType):
        gLog.print("MsgHandler {0}: adding interest {1}".format(self.name, msgType))
        assert(msgType in ControllerInMsgs)
        self.outQu.put(AddInterestMsg(inQuNum = self.inQuNum, interest = msgType))
        #gLog.print("MsgHandler {0}: finished adding interest {1}".format(self.name, msgType))

    def removeInterest(self, msgType):
        gLog.print("MsgHandler {0}: removing interest {1}".format(self.name, msgType))
        assert(msgType in ControllerInMsgs)
        self.outQu.put(RemoveInterestMsg(inQuNum = self.inQuNum, interest = msgType))

    def close(self):
        gLog.print("MsgHandler {0}: closing and removing all interests".format(self.name))
        self.outQu.put(RemoveAllInterestsMsg(inQuNum = self.inQuNum))

    def getBlocking(self):
        msg = self.inQu.get()
        #gLog.print("MsgHandler {0}: getBlocking message {1}".format(self.name, msg))
        return msg

    def getNonblocking(self):
        try:
            msg = self.inQu.get(False) # non-blocking
            #gLog.print("MsgHandler {0}: getNonblocking message {1}".format(self.name, msg))
            return msg
        except multiprocessing.queues.Empty:
            gLog.print("MsgHandler {0}: getNonblocking empty qu exception)".format(self.name))
            raise multiprocessing.queues.Empty

    def put(self, msg):
        #gLog.print("MsgHandler {0}: putting message {1}".format(self.name, msg))
        self.outQu.put(msg)

    def waitFor(self, msg):
        #gLog.print("waitFor: msg = {0}".format(msg))
        assert(isinstance(msg, tuple))
        m = None      # Giving m an arbitrary value just to fool the code inspection
        while True:
            while True:
                #gLog.print("waitFor: trying to get from qu")
                m = self.inQu.get()
                #gLog.print("waitFor: back from qu with message {0}".format(m))
                if type(m) == type(msg):
                    break
            if isinstance(m, PutReadLayoutResponseMsg) or isinstance(m, PutPathMsg) or isinstance(m, PutTrainPositionMsg):
                break
            if isinstance(m, InputRepMsg) and m.sensor == msg.sensor:
                break
            if isinstance(m, PutInitOutcomeMsg) and m.physAdd == msg.physAdd:
                break
            if isinstance(m, PutSensorStateMsg) and m == msg:    # matches iff sensor number and state match
                break
        #gLog.print("waitFor: waiting over, got message {0}".format(m))
        return m

################################################################################


class MsgPumpHandler(object):
    def __init__(self, name = None, host = None, port = None, inQuList = None, outQu = None):
        gLog.print("MsgPumpHandler {0}: initializing".format(name))
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
        #gLog.print("MsgHandler {0}: trying to start pumps".format(self.name))
        internalQu = Queue()
        msgSock = MsgSocket(name = self.name)
        msgSock.connect(self.host, self.port)
        MsgInternalQuPump(name = self.name, internalQu = internalQu, inQuList = self.inQuList).start()
        MsgInQuPump(name = self.name, sock = msgSock, inQuList = self.inQuList).start()
        MsgOutQuPump(name = self.name, sock = msgSock, outQu = self.outQu, internalQu = internalQu).start()
        gLog.print("MsgHandler {0}: has started pumps".format(self.name))

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
        gLog.print("MsgInternalQuPump {0}: initializing".format(name))
        self.name = name
        self.internalQu = internalQu
        self.inQuList = inQuList

    def run(self):           
        gLog.print("MsgInternalQuPump {0}: starting".format(self.name))
        while True:
            msg = self.internalQu.get()  
            if isinstance(msg, RemoveAllInterestsMsg):
                gLog.print("MsgInternalQuPump {0}: removing all interests for inQuNum {1}".
                           format(self.name, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # else remove all interests for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    interestList = self.inQuList[msg.inQuNum].interests
                    for interest in interestList:
                        interestList.remove(interest)
            elif isinstance(msg, AddInterestMsg):
                gLog.print("MsgInternalQuPump {0}: adding interest {1} for inQuNum {2}".
                           format(self.name, msg.interest, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # elif the inQuNum already has this interest pass
                # else add an interest for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    x = self.inQuList[msg.inQuNum]
                    if msg.interest in x.interests:
                        pass
                        # The interest could have been have added at a higher level
                        #raise Exception("MsgInternalQuPump {0}: Can't add. inQuNum {1} already has {2}".
                        #                format(self.name, msg.inQuNum, msg.interest))
                    else:
                        x.interests.append(msg.interest)
            elif isinstance(msg, RemoveInterestMsg):
                gLog.print("MsgInternalQuPump {0}: removing interest {1} for inQuNum {2}".
                           format(self.name, msg.interest, msg.inQuNum))
                # if there is no entry for this inQuNum raise an exception
                # elif there is no matching interest raise an exception
                # else remove an interest for this inQuNum
                if len(self.inQuList) <= msg.inQuNum:
                    raise Exception("MsgInternalQuPump {0}: inQuNum {1} is too large.".format(self.name, msg.inQuNum))
                else:
                    x = self.inQuList[msg.inQuNum]
                    if not msg.interest in x.interests:
                        raise Exception("MsgInternalQuPump {0}: Can't remove. inQuNum {1} doesn't have {2}".
                                        format(self.name, msg.inQuNum, msg.interest))
                    else:
                        x.interests.remove(msg.interest)
            else:
                gLog.print("MsgInternalQuPump {0}: unrecognized message {1}}".format(self.name, msg))


################################################################################

class MsgOutQuPump(Thread):
    def __init__(self, name = None, sock = None, outQu = None, internalQu = None):
        Thread.__init__(self)
        assert(isinstance(name, str))
        assert(isinstance(sock, MsgSocket))
        assert(isinstance(outQu, multiprocessing.queues.Queue))
        assert(isinstance(internalQu, multiprocessing.queues.Queue))
        gLog.print("MsgOutQuPump {0}: initializing".format(name))
        self.name = name
        self.sk = sock
        self.outQu = outQu
        self.internalQu = internalQu

    def run(self):
        gLog.print("MsgOutQuPump {0}: starting".format(self.name))
        while True:
            msg = self.outQu.get()
            if type(msg) in InQuListMsgs:
                #gLog.print("MsgOutQuPump {0}: getting ready to put {1}".format(self.name, msg))
                self.internalQu.put(msg)
            else:
                #gLog.print("MsgOutQuPump {0}: getting ready to send {1}".format(self.name, msg))
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
        gLog.print("MsgInQuPump {0}: initializing".format(name))
        self.name = name
        self.sk = sock
        self.inQuList = inQuList

    def run(self):
        gLog.print("MsgInQuPump {0}: starting".format(self.name))
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
           msk.connect(serverSideHost, serverSidePort) # Tries for 5 seconds before throwing an exception
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
        gLog.print("MsgSocket {0}: initializing".format(name))
        self.name = name
        self.inBuffer = None
        self.sock = None

    def connect(self, host, port):
        assert(isinstance(host, str))
        assert(port > 0)
        gLog.print("MsgSocket {0}: is trying to connect to ({1}, {2})".format(self.name, host, port))
        self.inBuffer = bytearray()
        self.sock = socket()
        for i in range(5):
            if not self.sock.connect_ex((host, port)):
                gLog.print("MsgSocket {0}: has connected to peer {1}".format(self.name, self.sock.getpeername()))
                return
            else:
                gLog.print("MsgSocket {0}: failed to connect to ({1}, {2})".format(self.name, host, port))
                sleep(1)
        st = "MsgSocket {0}: gave up trying to connect to ({1}, {2})".format(self.name, host, port)
        gLog.print(st)
        raise Exception(st)

    def attach(self, sock):
        assert(isinstance(sock, socket))
        gLog.print("MsgSocket {0}: has attached to peer {1}".format(self.name, sock.getpeername()))
        self.sock = sock
        self.inBuffer = bytearray()

    def send(self, msg):
        assert(isinstance(msg, tuple))
        st = makeMsgStr(msg)
        self.sock.sendall(st)
        gLog.print("<<< Sent    {0}    to peer {1}".format(msg, self.sock.getpeername()))

    def close(self):
        gLog.print("MsgSocket {0}: closing ".format(self.name))
        self.sock.close()

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            for x in buf:
                self.inBuffer.append(x)
        strSize = self.inBuffer[0] + 128 * self.inBuffer[1]
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)         # WARNING: if length of buf is 0, then the connection has been broken
            for x in buf:
                self.inBuffer.append(x)
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        gLog.print("    >>> Received    {0}    from peer {1}".format(msg, self.sock.getpeername()))
        return msg


class MsgServerThread(Thread):
    def __init__(self, name = "1", host = None, port = None, clientHandlerFunction = None):
        Thread.__init__(self)
        assert(host is None or isinstance(name, str))
        assert(isinstance(host, str))
        assert(isinstance(port, int))
        assert(clientHandlerFunction is not None)
        gLog.print("Message server {0}: initializing at ({1}, {2})".format(name, host, port))
        self.name = name
        if host is None:
            host = socket.gethostname()
        self.host = host
        self.port = port
        self.clientHandlerFunction = clientHandlerFunction
        self.serverSocket = None

    def run(self):
        gLog.print("Message server {0}: running".format(self.name))
        self.serverSocket = socket()
        self.serverSocket.bind((self.host, self.port))           # bind
        self.serverSocket.listen(5)                              # listen
        gLog.print("Message server {0}: now listening at ({1}, {2})".format(self.name, self.host, self.port))
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            gLog.print("Message server {0}: just created a connection to peer at {1}".
                       format(self.name, socketToClient.getpeername()))
            ClientHandlerThread("1", socketToClient, self.clientHandlerFunction).start()


class ClientHandlerThread(Thread):
    def __init__(self, name, socketToClient, clientHandlerFunction):
        Thread.__init__(self)
        assert(isinstance(name, str))
        assert(isinstance(socketToClient, socket))
        assert(clientHandlerFunction is not None)
        gLog.print("Client handler {0}: created for peer {1}".format(name, socketToClient.getpeername()))
        self.name = name
        self.socketToClient = socketToClient
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        msgSocketToClient = MsgSocket()
        msgSocketToClient.attach(self.socketToClient)
        gLog.print("Client handler {0}: running with peer {1} and function {2}"
                   .format(self.name, self.socketToClient.getpeername(), self.clientHandlerFunction))
        self.clientHandlerFunction(msgSocketToClient)



