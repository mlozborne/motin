from MessageTranslationTypes import *
from Log import printLog
import queue
import multiprocessing

class Throttle(object):
    def __init__(self, nm, quToCon):
        printLog("Initializing Throttle      ...in Throttle")
        assert(isinstance(nm, str))
        assert(isinstance(quToCon, queue.Queue) or isinstance(quToCon, multiprocessing.queues.Queue))
        self.quToCon = quToCon
        self.virtSlot = None
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOff
        self.F5 = 0
        self.F6 = 0

    def setVirtSlot(self, virtSlot):
        self.virtSlot = virtSlot
		
    def do(self, func, * args):
        func(self, * args)

    def sendDirf(self):
        self.quToCon.put(LocoDirfMsg(slot=self.virtSlot, direction=self.direction,
                     lights=self.lights, horn=self.horn, bell=self.bell))

    def sendSnd(self):
        self.quToCon.put(LocoSndMsg(slot=self.virtSlot, mute=self.mute,
                     F5=self.F5, F6=self.F6))

    def setSpeed(self, speed):
        self.quToCon.put(LocoSpdMsg(slot=self.virtSlot, speed=speed))

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
        self.quToCon.put(SwReqMsg(switch=id, direction=direction))



			
