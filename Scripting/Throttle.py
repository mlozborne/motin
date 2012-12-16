from MessageTranslationTypes import *
from MessageTranslationLibrary import *
from Log import printLog
from TCP import *
#from functools import partial

class Throttle(object):
    def __init__(self, sock):
        printLog("Initializing Throttle ")
        self.sk = sock
        self.virtSlot = None
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOff
        self.F5 = 0
        self.F6 = 0

    def doLocoInit(self, locoAddress, sensors):
        """
        Won't work because throttle should not receive directly           <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        """
        self.sk.send(DoLocoInitMsg(address=locoAddress, sensors=sensors))
        msg = self.sk.receive()
        while not isinstance(msg, PutInitOutcomeMsg):
            msg = self.sk.receive()
        self.virtSlot = msg.virtSlot
        return msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot
		
    def do(self, func, * args):
        func(self, * args)

    def sendDirf(self):
        self.sk.send(LocoDirfMsg(slot=self.virtSlot, direction=self.direction,
                     lights=self.lights, horn=self.horn, bell=self.bell))

    def sendSnd(self):
        self.sk.send(LocoSndMsg(slot=self.virtSlot, mute=self.mute,
                     F5=self.F5, F6=self.F6))

    def setSpeed(self, speed):
        self.sk.send(LocoSpdMsg(slot=self.virtSlot, speed=speed))

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
        self.sendDirf()

    def setMute(self, onOff):
        self.mute = onOff
        self.sendSnd()

    def closeNextSwitch(self):
        if self.F5 == 0:
            self.F5 = 1
        else:
            self.F5 = 0
        self.sendSnd()

    def throwNextSwitch(self):
        if self.F6 == 0:
            self.F6 = 1
        else:
            self.F6 = 0
        self.sendSnd()

    def moveSwitch(self, id, direction):
        self.sk.send(SwReqMsg(switch=id, direction=direction))



			
