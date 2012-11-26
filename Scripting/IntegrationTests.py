##########################################################################################
"""
Test TCP basics
"""
import time
from MessageTranslationLibrary import *
from TCP import *
import subprocess
from threading import Thread
		
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
	
class BlinkLightsDirectly(Thread):
	def __init__(self, sk):
		Thread.__init__(self, name = "BlinkLightsDirectly")
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
		time.sleep(5)
		subprocess.call("tskill railroadbig", shell=True)                       # for XP
		subprocess.call("tskill StartController", shell=True)                   # for XP

def testReceive():
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	sk = RailSocket('localhost', 1234)
	process = BlinkLightsDirectly(sk)
	process.start()
	while True:
		print sk.receive()
		
class BlinkLightsViaController(Thread):
	def __init__(self, sk):
		Thread.__init__(self, name = "BlinkLightsViaController")
		self.sk = sk
		
	def run(self):
		self.sk.send(DoLocoInitMsg(address=1111,sensors=[5,1]))
		time.sleep(1)
		for i in range(1, 6):
			self.sk.send(LocoDirfMsg(slot=5, direction=kBackward, lights=kOn, horn=kOff, bell=kOn))
			time.sleep(0.1)
			self.sk.send(LocoDirfMsg(slot=5, direction=kForward, lights=kOff, horn=kOff, bell=kOff))
			time.sleep(0.1)

def testTalkingToController():
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	subprocess.call("start ../runSoftware/StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes", shell=True)
	subprocess.call("start ../runSoftware/AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE ../runSoftware/Layout.xml KEYBOARDLOG no ADMINLOG yes", shell=True)
	sk = RailSocket('localhost', 1235)
	process = BlinkLightsViaController(sk)
	time.sleep(10)
	process.start()
	while True:
		print sk.receive()
	