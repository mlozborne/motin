from socket import *
from MessageTranslationLibrary import *
import time

class RailSocket:
	def __init__(self, host, port):
		self.inBuffer = ""
		self.sock = socket(AF_INET, SOCK_STREAM)
		while True:
			if not self.sock.connect_ex((host,port)): break
			time.sleep(1)
		
	def send(self, msg):
		self.sock.sendall(makeMsgStr(msg))
		
	def close(self):
		self.sock.close()

	def receive(self):
		if len(self.inBuffer) < 2:
			buf = self.sock.recv(1024)
			self.inBuffer += buf
		strSize = ord(self.inBuffer[0]) + 128 * ord(self.inBuffer[1])
		while strSize + 2 > len(self.inBuffer):
			buf = self.sock.recv(1024)
			self.inBuffer += buf
		strMsg = self.inBuffer[2:2+strSize]
		self.inBuffer = self.inBuffer[2+strSize:]
		return splitMsgStr(strMsg)
		