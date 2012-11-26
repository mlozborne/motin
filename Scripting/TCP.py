from socket import *
from MessageTranslationLibrary import *
import time

class RailSocket:
	def __init__(self, host, port):
		self.sock = socket(AF_INET, SOCK_STREAM)
		while True:
			if not self.sock.connect_ex((host,port)): break
			time.sleep(1)
		
	def send(self, msg):
		self.sock.sendall(makeMsgStr(msg))
		
	def close(self):
		self.sock.close()

	def receive(self):
		st = self.sock.recv(1024)
		return splitMsgStr(st[2:])