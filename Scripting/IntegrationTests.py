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

def waitForSensor(sk, id, state):
	print "Waiting for sensor {0} to enter state {1}".format(id, state)
	msg = sk.receive()
	while not isinstance(msg, PutSensorStateMsg) or msg.id != id or msg.state != state:
		msg = sk.receive()

def testControllingTrain():
	print "Starting simulator and controller"
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	time.sleep(1)
	subprocess.call("start ../runSoftware/StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes", shell=True)
	#subprocess.call("start ../runSoftware/AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE ../runSoftware/Layout.xml KEYBOARDLOG no ADMINLOG yes", shell=True)
	time.sleep(3)
	
	print "Connect a socket to the controller"
	sk = RailSocket('localhost', 1235)
	
	sk.send(DoReadLayoutMsg(fileName = "../runSoftware/Layout.xml"))
	msg = sk.receive()
	while not isinstance(msg, PutReadLayoutResponseMsg):
		msg = sk.receive()
	time.sleep(3)
		
	if msg.responseFlag != 1:
		print "ABEND"
		print "Error in XML file with flag = " + repr(msg.responseFlag) + " and code = " + repr(msg.code)
		print "THE END"
		return
	
	#Initiliazing train 1111
	tr = Train(1111, sk)
	if not tr.doLocoInit([5, 1]):
		print "ABEND: couldn't initialize the train"
		return	
		
	tr.setSpeed(100)	
	sk.send(SwReqMsg(switch = 4, direction = kThrown))
	time.sleep(2)
	
	speed1 = 100
	speed2 = 50
	for i in range(0, 1):
		tr.setDirection(kForward)
		speed1 -= 3
		tr.setSpeed(speed1)
		time.sleep(3)	
		
		tr.setSpeed(0)		
		time.sleep(1)
		
		tr.setDirection(kBackward)
		speed2 -= 3
		tr.setSpeed(speed2)
		time.sleep(3)

		tr.setSpeed(0)		
		time.sleep(1)
		
		
	tr.setLights(kOn)
	tr.setDirection(kForward)
	tr.setSpeed(100)
	time.sleep(3)
	tr.setSpeed(0)
	#time.sleep(1)
"""	for i in range(0,3):
		tr.setLights(kOff)
		time.sleep(1)
		tr.setLights(kOn)
		time.sleep(1) """
		
	
	
	
	
	
	
	
	
	#sk.send(LocoSpdMsg(slot = vslot, speed = 50))
	
	#time.sleep(1)
	#print "Waiting 10 seconds and sending the train back"
	#time.sleep(10)
	#sk.send(LocoDirfMsg(slot = vslot, direction = kBackward, lights = kOn, horn = kOff, bell = kOff))
	#
	


	
		
