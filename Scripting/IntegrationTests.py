"""
Test TCP basics
"""
import time
from MessageTranslationLibrary import *
from TCP import *
import os
import subprocess
		
def testSend():
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	sk = RailSocket('localhost', 1234)
	sk.send(LocoAdrMsg(address=1111))
	time.sleep(1)
	sk.send(MoveSlotsMsg(slot1=1, slot2=1))
	time.sleep(1)
	for i in range(1, 6):
		sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
		time.sleep(0.1)
		sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
		time.sleep(0.1)
	sk.close()
	subprocess.call("tskill railroadbig", shell=True)                       # for XP
	subprocess.call("taskkill /T /IM railroadbig.exe", shell=True)          # for Windows 7
	
from threading import Thread
class BlinkLights(Thread):
	def __init__(self, sk):
		Thread.__init__(self, name = "BlinkLights")
		self.sk = sk
		
	def run(self):
		self.sk.send(LocoAdrMsg(address=1111))
		time.sleep(1)
		self.sk.send(MoveSlotsMsg(slot1=1, slot2=1))
		time.sleep(1)
		for i in range(1, 6):
			self.sk.send(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
			time.sleep(0.1)
			self.sk.send(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
			time.sleep(0.1)

def testReceive():
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	sk = RailSocket('localhost', 1234)
	process = BlinkLights(sk)
	process.start()
	time.sleep(5)
	while True:
		print sk.receive()