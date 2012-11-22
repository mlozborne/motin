from socket import *

class RailSocket:
	def __init__(self, host, port):
		self.sock = socket(AF_INET, SOCK_STREAM)
		self.sock.connect((host,port))
		
	def send(self, str):
		self.sock.send(str)
		
	def close(self):
		self.sock.close()

	
#########################################################

import unittest
import time
from MessageTranslationLibrary import *

class TestTCP(unittest.TestCase):
	def testSendStr(self):
		rs = RailSocket('localhost', 1234)
		for i in range(1, 6):
			msg = makeLocoDirfStr(LocoDirfMsg(slot=11, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
			rs.send(msg)
			time.sleep(1)
			msg = makeLocoDirfStr(LocoDirfMsg(slot=11, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
			rs.send(msg)
			time.sleep(1)
		rs.close()
		self.assertEquals(1, 1)
		
def runTest():
	suite = unittest.makeSuite(TestMessageTranslationLibrary)
	unittest.TextTestRunner().run(suite)
	
def main():
		rs = RailSocket('localhost', 1234)
		for i in range(1, 6):
			msg = makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
			rs.send(msg)
			time.sleep(1)
			msg = makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
			rs.send(msg)
			time.sleep(1)
		rs.close()
