from socket import *

class RailSocket:
	def __init__(self, host, port):
		self.sock = socket(AF_INET, SOCK_STREAM)
		self.sock.connect((host,port))
		
	def send(self, str):
		self.sock.send(str)
		
	def close(self):
		self.sock.close()
