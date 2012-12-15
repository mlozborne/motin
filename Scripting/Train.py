from functools import partial
from socket import *
import time

from MessageTranslationLibrary import *

class Train:
    def __init__(self, address, sock):
        print "Initializing train " + repr(address)
        self.address = address
        self.sk = sock
        self.slot = 0
        self.speed = 0
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOn
        self.F5 = 0
        self.F6 = 0
        self.sensors = []
        self.state = kHalted
		
    def do(self, func, * args):
        func(self, * args)
	
    def sendDirf(self):
        self.sk.send(LocoDirfMsg(slot=self.slot, direction=self.direction,
                     lights=self.lights, horn=self.horn, bell=self.bell))
	
	
    def doLocoInit(self, sensors):
        self.sensors = sensors
        self.sk.send(DoLocoInitMsg(address=self.address, sensors=self.sensors))
        msg = self.sk.receive()
        while not isinstance(msg, PutInitOutcomeMsg):
            msg = self.sk.receive()
        self.slot = msg.virtSlot
        return msg.physSlot
		
    def setSpeed(self, speed):
        self.speed = speed
        self.sk.send(LocoSpdMsg(slot=self.slot, speed=self.speed))
		
    def setDirection(self, direction):
        self.direction = direction
        self.sendDirf()
	
    def setLights(self, onOff):
        self.lights = onOff
        self.sendDirf()

    def setHorn(self, onOff):
        self.horn = onOff
        self.sendDirf()

    def setBell(self, onOff):
        self.bell = onOff
        self.sk.send(LocoDirfMsg(slot=self.slot, direction=self.direction,
                     lights=self.lights, horn=self.horn, bell=self.bell))
    def setMute(self, onOff):
        self.mute = onOff
        self.sk.send(LocoSndMsg(slot=self.slot, mute=self.mute,
                     F5=self.F5, F6=self.F6))
										
    def updateFromMessage(self, msg):
        if isinstance(msg, PutTrainStateMsg) and msg.slot == self.slot:
            self.state = msg.state
        elif isinstance(msg, PutTrainPositionMsg) and msg.slot == self.slot:
            self.sensors = msg.sensors
        elif isinstance(msg, PutTrainInformationMsg) and msg.slot == self.slot:
            self.speed = msg.speed
            self.direction = msg.direction
            self.lights = msg.lights
            self.bell = msg.bell
            self.horn = msg.horn
            self.mute = msg.mute
						
    def waitForSensor(self, id):
        while True:
            msg = self.sk.receive()
            if isinstance(msg, PutTrainPositionMsg):
                if self.slot == msg.slot and id in (msg.sensors)[1:]:
                    return
			
		
#########################################################################
################  Unit Testing                  #########################
#########################################################################
