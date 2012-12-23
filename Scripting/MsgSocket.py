from socket import socket
from time import sleep
from MessageTranslationLibrary import makeMsgStr, splitMsgStr
from Log import printLog
from threading import Thread
import sys

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
              this function should close its socket when done
           msk = MessageSocket()
           msk.createMsgServerThread(serverSideHost, serverSidePort, clientHandlerFunction)


    """
    def __init__(self):
        printLog("Creating a MsgSocket")

    def createMsgServerThread(self, host, port, clientHandlerFunction):
        printLog("Starting a server at ({0}, {1})".format(host, port))
        MsgServerThread(host, port, clientHandlerFunction).start()

    def connect(self, host, port):
        printLog("A client is trying to connect to ({0}, {1})".format(host, port))
        self.inBuffer = []
        self.sock = socket()
        for i in range(10):
            if not self.sock.connect_ex((host, port)):
                printLog("A client has connected to {0}".format(self.sock.getpeername()))
                return
            sleep(1)
        st = "EXCEPTION: A client FAILED to connected to ({0}, {1})".format(host, port)
        printLog(st)
        print(st); sys.stdout.flush()
        raise

    def setup(self, sock):
        printLog("Standard socket = {0} has being attached to a MsgSocket".format(sock.getpeername()))
        self.sock = sock
        self.inBuffer = []

    def send(self, msg):
        st = makeMsgStr(msg)
        ba = bytes(st)
        self.sock.sendall(ba)
        printLog("<<< Sent message = {0}....to {1}".format(msg, self.sock.getpeername()))

    def close(self):
        printLog("Closing MsgSocket {0}".format(self.sock.getpeername()))
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
        printLog("    >>> Received {0}....from {1}".format(msg, self.sock.getpeername()))
        return msg


class MsgServerThread(Thread):
    def __init__(self, host, port, clientHandlerFunction):
        Thread.__init__(self)
        self.host = host
        self.port = port
        self.clientHandlerFunction = clientHandlerFunction
        self.serverSocket = socket()
        self.serverSocket.bind((host, port))                     # bind
        self.serverSocket.listen(5)                              # listen
        printLog("Server socket now listening at ({0}, {1})".format(host, port))

    def run(self):
        printLog("Entering server thread run method")
        while True:
            socketToClient, address = self.serverSocket.accept() # accept
            printLog("Server {0} just created a connection to client at {1}". \
                      format((self.host, self.port), socketToClient.getpeername()))
            ClientHandlerThread(socketToClient, self.clientHandlerFunction).start()

class ClientHandlerThread(Thread):
    def __init__(self, socketToClient, clientHandlerFunction):
        printLog("Creating a handler to client at {0}".format(socketToClient.getpeername()))
        Thread.__init__(self)
        self.socketToClient = socketToClient
        self.clientHandlerFunction = clientHandlerFunction

    def run(self):
        msgSocketToClient = MsgSocket()
        msgSocketToClient.setup(self.socketToClient)
        printLog("Client handler calling {0}".format(self.clientHandlerFunction))
        self.clientHandlerFunction(msgSocketToClient)

