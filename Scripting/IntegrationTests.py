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
		print "  Received the message: {0}".format(msg)
		msg = sk.receive()

def testControllingTrain():
	print "Starting simulator and controller"
	subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
	subprocess.call("start ../runSoftware/StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes", shell=True)
	#subprocess.call("start ../runSoftware/AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE ../runSoftware/Layout.xml KEYBOARDLOG no ADMINLOG yes", shell=True)
	time.sleep(5)
	
	print "Connect a socket to the controller"
	sk = RailSocket('localhost', 1235)
	
	print "Sending DoReadLayoutMsg and waiting for the PutReadLayoutResponseMsg response"
	sk.send(DoReadLayoutMsg(fileName = "../runSoftware/Layout.xml"))
	msg = sk.receive()
	while not isinstance(msg, PutReadLayoutResponseMsg):
		print "  Received the message: " + repr(msg)
		msg = sk.receive()
	time.sleep(5)
		
	print "Checking the response"
	if msg.responseFlag != 1:
		print "ABEND"
		print "Error in XML file with flag = " + repr(msg.responseFlag) + " and code = " + repr(msg.code)
		print "THE END"
		return
	
	#Initiliazing train 1111
	tr = Train(1111, sk)
	if not tr.doLocoInit([5, 1]):
		print "ABEND: couldn't initiaze the train"
		return	
		
	print "Starting train 1111 with a speed of 100 and lights on"
	tr.setLights(kOn)
	tr.setSpeed(100)
	
	time.sleep(3)
	
	print "Throwing turnout 4"
	sk.send(SwReqMsg(switch = 4, direction = kThrown))
	
	waitForSensor(sk, 59, kSensorOpen)
		
	print "Sensor 59 fired. Stopping train 1111, reversing, and turning off lights"
	tr.setSpeed(0)
	tr.setDirection(kBackward)
	tr.setSpeed(60)
	
	return
	
	
	
	
	
	
	
	
	#sk.send(LocoSpdMsg(slot = vslot, speed = 50))
	
	#time.sleep(1)
	#print "Waiting 10 seconds and sending the train back"
	#time.sleep(10)
	#sk.send(LocoDirfMsg(slot = vslot, direction = kBackward, lights = kOn, horn = kOff, bell = kOff))
	#
	


	
		
