"""
Test TCP basics
"""
import time
from MessageTranslationLibrary import *
from TCP import *
		
def testSend():
	sk = RailSocket('localhost', 1234)
	for i in range(1, 6):
		msg = makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
		sk.send(msg)
		time.sleep(1)
		msg = makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
		sk.send(msg)
		time.sleep(1)
	sk.close()
	
