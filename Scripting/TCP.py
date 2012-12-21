
from socket import socket
from time import sleep
from Log import printLog

from MessageTranslationLibrary import *

class RailSocket(object):
    def __init__(self, host, port):
        self.inBuffer = []
        printLog("Trying to create socket               ...in TCP")
        self.sock = socket()
        #self.sock.setblocking(1)
        printLog("Socket created = {0}                  ...in TCP".format(self.sock))
        while True:
            if not self.sock.connect_ex((host, port)): break
            sleep(1)
        printLog("Connected to IP = {0}, port = {1}     ...in TCP".format(host, port))
		
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

       
