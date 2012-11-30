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
	
########################################################################

from Train import *
from Layout import *

def stopTrain(self):
	self.setSpeed(0)
	self.setLights(kOff)
	
def blink(self, n):
	print "\nIn blink n = {0}\n".format(n)
	for i in range(0, 3):
		self.setLights(kOn)
		time.sleep(1)
		self.setLights(kOff)
		time.sleep(1)

def testControllingTrain():
	print "Starting simulator and controller"
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	time.sleep(1)
	subprocess.call("start ../runSoftware/StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes", shell=True)
	#subprocess.call("start ../runSoftware/AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE ../runSoftware/Layout.xml KEYBOARDLOG no ADMINLOG yes", shell=True)
	time.sleep(3)
	
	print "\nConnect a socket to the controller"
	sk = RailSocket('localhost', 1235)
	
	print "\nRead the layout file"
	responseFlag, code = readLayoutFile(sk, "../runSoftware/Layout.xml")
	print "responseFlag = {0} and code ={1}".format(responseFlag, code)
	if responseFlag != 1:
		print "ABEND"
		print "Error in XML file with flag = " + repr(msg.responseFlag) + " and code = " + repr(msg.code)
		print "THE END"
		return	
	
	print "\nCommand the train"
	tr = Train(1111, sk)
	response = tr.doLocoInit([5, 1])
	if response > 120:
		print "\nABEND: couldn't initialize the train. Response code = {0}".format(response)
		return		
	tr.do(blink, 4)
	tr.setSpeed(100)	
	sk.send(SwReqMsg(switch = 4, direction = kThrown))
	tr.waitForSensor(62)
	func = stopTrain
	tr.do(stopTrain)
	
	time.sleep(1)

	


	
		
