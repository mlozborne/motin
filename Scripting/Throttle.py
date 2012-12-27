from MessageTranslationTypes import *
from Log import printLog
import queue
import multiprocessing
from MsgHandler import waitFor
from time import sleep

class Throttle(object):
    def __init__(self, nm = None, inQu = None, outQu = None):
        printLog("Throttle {0} initializing".format(nm))
        assert(isinstance(nm, str))
        assert(isinstance(inQu, queue.Queue) or
               isinstance(outQu, multiprocessing.queues.Queue))
        assert(isinstance(outQu, queue.Queue) or
               isinstance(outQu, multiprocessing.queues.Queue))
        self.inQu = inQu
        self.outQu = outQu
        self.virtSlot = None
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOff
        self.F5 = 0
        self.F6 = 0

    def readLayout(self, fileName):
        printLog("Throttle sending DoReadLayoutMsg using file {0}".format(fileName))
        assert(self.outQu != None)
        assert(self.inQu != None)
        assert(isinstance(fileName, str))
        self.outQu.put(DoReadLayoutMsg(fileName = fileName))
        msg = waitFor(self.inQu, PutReadLayoutResponseMsg(responseFlag = 0, code = 0))
        sleep(3)
        return msg

    def initTrain(self, locoAddress, position):
        printLog("Throttle sending DoLocoInitMsg")
        assert(self.outQu != None)
        assert(self.inQu != None)
        assert(0 <= locoAddress <= 9999)
        assert(isinstance(position, list) or isinstance(position, tuple))
        self.outQu.put(DoLocoInitMsg(address = locoAddress, sensors = position))
        msg = waitFor(self.inQu, PutInitOutcomeMsg(physAdd = locoAddress, physSlot = 0, virtAdd = 0, virtSlot = 0))
        if msg.physSlot > 120:
            self.virtSlot = None
        else:
            self.virtSlot = msg.virtSlot
        return msg

    def setVirtSlot(self, virtSlot):
        self.virtSlot = virtSlot
		
    def do(self, func, * args):
        func(self, * args)

    def sendDirf(self):
        assert(self.outQu != None)
        assert(self.virtSlot != None)
        self.outQu.put(LocoDirfMsg(slot=self.virtSlot, direction=self.direction,
                        lights=self.lights, horn=self.horn, bell=self.bell))

    def sendSnd(self):
        assert(self.outQu != None)
        assert(self.virtSlot != None)
        self.outQu.put(LocoSndMsg(slot=self.virtSlot, mute=self.mute,
                        F5=self.F5, F6=self.F6))

    def setSpeed(self, speed):
        assert(self.outQu != None)
        assert(self.virtSlot != None)
        assert(speed >= 0)
        self.outQu.put(LocoSpdMsg(slot=self.virtSlot, speed=speed))

    def setDirection(self, direction):
        assert(direction == kForward or direction == kBackward)
        self.direction = direction
        self.sendDirf()

    def setLights(self, onOff):
        assert (onOff == kOn or onOff == kOff)
        self.lights = onOff
        self.sendDirf()

    def setHorn(self, onOff):
        assert (onOff == kOn or onOff == kOff)
        self.horn = onOff
        self.sendDirf()

    def setBell(self, onOff):
        assert (onOff == kOn or onOff == kOff)
        self.bell = onOff
        self.sendDirf()

    def setMute(self, onOff):
        assert (onOff == kOn or onOff == kOff)
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
        assert(self.outQu != None)
        assert(id > 0)
        assert(direction == kForward or direction == kBackward)
        self.outQu.put(SwReqMsg(switch=id, direction=direction))



			
