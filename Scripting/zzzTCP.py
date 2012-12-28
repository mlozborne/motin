
from socket import socket
from time import sleep
from Log import printLog

from MessageTranslationLibrary import *

class RailSocket(object):
    def __init__(self, name = "1", host = None, port = None):
        printLog("RailSocket {0}: initializing  ...in TCP".format(name))
        assert(isinstance(name, str))
        assert(isinstance(host, str))
        assert(port > 1000)
        self.name = name
        self.inBuffer = []
        self.sock = socket()
        while True:
            if not self.sock.connect_ex((host, port)): break
            sleep(1)
        printLog("RailSocket {0}: connected to host = {1}, port = {2}  ...in TCP".format(self.name, host, port))
		
    def send(self, msg):
        st = makeMsgStr(msg)
        ba = bytes(st)
        self.sock.sendall(ba)
        printLog("<<< Sent message = {0}".format(msg))
		
    def close(self):
        self.sock.close()
        printLog("RailSocket {0}: closed ...in TCP".format(self.name))

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

       
