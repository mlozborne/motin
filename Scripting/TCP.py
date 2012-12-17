
from socket import *
import time
from Log import printLog

from MessageTranslationLibrary import *

class RailSocket(object):
    def __init__(self, host, port):
        self.inBuffer = ""
        printLog("TCP: Trying create socket")
        self.sock = socket()
        xxxxx#self.sock.setblocking(True)
        printLog("TCP: Socket created = {0}".format(self.sock))
        while True:
            if not self.sock.connect_ex((host, port)): break
            time.sleep(1)
        printLog("TCP: Connect to IP = {0}, port = {1}".format(host, port))
		
    def send(self, msg):
        st1 = makeMsgStr(msg)
        xxxxxx st2 = st1.encode('utf-8')  #unicodes larger than 127 are converted into two bytes
        if len(st2) > len(st1):
            st2 = st2[:2] + st2[3:]
        printLog("<<< Sent message = {0}, st1 = {1}, st2 = {2}".format(msg, list(st1), list(st2)))
        self.sock.sendall(st2)
		
    def close(self):
        self.sock.close()
        printLog("TCP: Closed RailSocket")

    def receive(self):
        if len(self.inBuffer) < 2:
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strSize = ord(self.inBuffer[0]) + 128 * ord(self.inBuffer[1])
        while strSize + 2 > len(self.inBuffer):
            buf = self.sock.recv(1024)
            self.inBuffer += buf
        strMsg = self.inBuffer[2:2 + strSize]
        self.inBuffer = self.inBuffer[2 + strSize:]
        msg = splitMsgStr(strMsg)
        printLog("    >>> Received {0}".format(msg))
        return msg

       
