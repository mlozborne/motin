from socket import socket
from time import sleep
from MessageTranslationLibary import makeMsgStr, splitMsgStr
from Log import printLog
from Threading import Thread

class MsgSocket(object):
    """
    This class is a wrapper for the standard socket class.
    1) Data is restricted to train messages as defined in the MessageTranslationType
       module.
    2) Train messages are automatically converted to ascii strings before being
       sent, and these ascii strings are automatically converted back to train messages on receipt. The
       ascii strings include a two byte prefix that indicates the length of the
       rest of the string.
    3) Usage pattern on the client side.
           msk = MessageSocket()
           msk.connect(serverSideHost, serverSidePort) # Tries for 10 seconds before throwing an exception
           ...
           msk.send(msg)
           ...
           msg = msk.receive()
           ...
           msk.close()
    4) Usage pattern on the server side
           define a clientHandlerFunction for use by the client handler
           msk = MessageSocket()
           msk.createMsgServerThread(serverSideHost, serverSidePort, clientHandlerFunction)

    """
    def __init__(self):
        pass

    def createMsgServerThread(self, host, port, clientHandlerFunction):
        MsgServerThread(self, host, port, clientHandlerFunction).start()

    def connect(self, host, port):
        self.inBuffer = []
        self.sock = socket()
        for i in range(10):
            if not self.sock.connect_ex((host, port)):
                return
            sleep(1)
        throw exception

    def setup(self, sock):
        self.sock = sock
        self.inBuffer = []

    def send(self, msg):
        st = makeMsgStr(msg)
        ba = bytes(st)
        self.sock.sendall(ba)
        printLog("<<< Sent message = {0}".format(msg))

    def close(self):
        self.sock.close()
        printLog("Closed RailSocket                  ...in TCP")

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
        printLog("    >>> Received {0}".format(msg))
        return msg


class MsgServerThread(Thread):
    def __init__(self, address, clientHandlerRunFunction):
        Thread.__init__(self)
        self.clientHandlerRunFunction = clientHandlerRunFunction
        self.serverSocket = socket()
        self.serverSocket.bind(address)                          # bind
        self.serverSocket.listen(5)                              # listen

    def run(self):
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            ClientHandlerThread(socketToClient, self.clientHandlerRunFunction).start()

class ClientHandlerThread(thread):
    def __init__(self, socketToClient, clientHandlerRunFunction):
        Thread.__init__(self)
        self.socketToClient = socketToClient
        self.clientHandlerRunFunction = clientHandlerRunFunction

    def run(self):
        msgSocketToClient = MsgSocket()
        msgSocketToClient.setup(self.socketToClient)
        clientHandlerFunction(msgSocketToClient)
        self.msgSocketToClient.close()

