from Log import printLog
from MessageTranslationTypes import *
from MessageTranslationTypes import ControllerInMsgs
from MessageTranslationTypes import DoLocoInitMsg
from MessageTranslationTypes import DoReadLayoutMsg
from MessageTranslationTypes import LocoDirfMsg
from MessageTranslationTypes import LocoSndMsg
from MessageTranslationTypes import LocoSpdMsg
from MessageTranslationTypes import PutInitOutcomeMsg
from MessageTranslationTypes import PutReadLayoutResponseMsg
from MessageTranslationTypes import SwReqMsg
from MessageTranslationTypes import kBackward
from MessageTranslationTypes import kForward
from MessageTranslationTypes import kOff
from MessageTranslationTypes import kOn
import MsgHandler
from MsgHandler import *
from MsgHandler import CommunicationsPackage
import multiprocessing
from time import sleep

class Throttle(object):
#    def __init__(self, name = None, inQu = None, inQuNum = None, outQu = None):
    def __init__(self, name = None, comPkg = None):
        printLog("Throttle {0}: initializing".format(name))

        assert(isinstance(name, str))

        assert(isinstance(comPkg, CommunicationsPackage))
        inQu = comPkg.inQu
        inQuNum = comPkg.inQuNum
        outQu = comPkg.outQu

#        print("inQu={0},inQuNum={1},outQu={2}".format(inQu,inQuNum,outQu))
        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(inQuNum >= 0)
        assert(isinstance(outQu, multiprocessing.queues.Queue))

        self.name = name
        self.inQu = inQu
        self.inQuNum = inQuNum
        self.outQu = outQu
        self.virtSlot = None
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOff
        self.F5 = 0
        self.F6 = 0
        self.msgHandler = MsgHandler(name = self.name, inQu = self.inQu, inQuNum = self.inQuNum, outQu = self.outQu)

    def addInterest(self, interest):
        printLog("Throttle {0}: adding interest {1}".format(self.name, interest))
        assert(interest in ControllerInMsgs)
        self.msgHandler.addInterest(interest)

    def removeInterest(self, interest):
        printLog("Throttle {0}: removing interest {1}".format(self.name, interest))
        assert(interest in ControllerInMsgs)
        self.msgHandler.removeInterest(interest)

    def close(self):
        printLog("Throttle {0}: closing".format(self.name))
        self.msgHandler.close()

    def getBlocking(self):
        msg = self.msgHandler.getBlocking()
        #printLog("Throttle {0}: getBlocking msg {1}".format(self.name, msg))
        return msg

    def getNonblocking(self):
        try:
            msg = self.msgHandler.getNonblocking()
            #printLog("Throttle {0}: getNonlocking msg {1}".format(self.name, msg))
            return msg
        except multiprocessing.queues.Empty:
            printLog("Throttle {0}: getNonlocking empty qu exception".format(self.name))
            raise multiprocessing.queues.Empty

    def waitFor(self, msg):
        printLog("waitFor: msg = {0}".format(msg))
        assert(isinstance(msg, tuple))
        return self.msgHandler.waitFor(msg)

################################################################################

    def readLayout(self, fileName):
        printLog("Throttle {0}: sending DoReadLayoutMsg using file {1}".format(self.name, fileName))
        assert(isinstance(fileName, str))
        self.msgHandler.addInterest(PutReadLayoutResponseMsg)
        self.msgHandler.put(DoReadLayoutMsg(fileName = fileName))
        msg = self.waitFor(PutReadLayoutResponseMsg(responseFlag = 0, code = 0))
        self.msgHandler.removeInterest(PutReadLayoutResponseMsg)
        sleep(3)
        return msg

    def initTrain(self, address, position):
        printLog("Throttle {0}: sending DoLocoInitMsg".format(self.name))
        assert(0 <= address <= 9999)
        assert(isinstance(position, list) or isinstance(position, tuple))
        self.msgHandler.put(DoLocoInitMsg(address = address, sensors = position))
        responseMsg = None
        if position != []:
            self.msgHandler.addInterest(PutInitOutcomeMsg)
            responseMsg = self.waitFor(PutInitOutcomeMsg(physAdd = address, physSlot = 0, virtAdd = 0, virtSlot = 0))
            self.msgHandler.removeInterest(PutInitOutcomeMsg)
            if responseMsg.physSlot > 120:
                self.virtSlot = None
            else:
                self.virtSlot = responseMsg.virtSlot
        return responseMsg

    def setVirtSlot(self, virtSlot):
        self.virtSlot = virtSlot
		
    def do(self, func, * args):
        func(self, * args)

    def sendDirf(self):
        assert(self.virtSlot != None)
        self.msgHandler.put(LocoDirfMsg(slot=self.virtSlot, direction=self.direction,
                        lights=self.lights, horn=self.horn, bell=self.bell))

    def sendSnd(self):
        assert(self.virtSlot != None)
        self.msgHandler.put(LocoSndMsg(slot=self.virtSlot, mute=self.mute,
                        F5=self.F5, F6=self.F6))

    def setSpeed(self, speed):
        assert(self.virtSlot != None)
        assert(speed >= 0)
        self.msgHandler.put(LocoSpdMsg(slot=self.virtSlot, speed=speed))

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
        assert(id > 0)
        assert(direction == kForward or direction == kBackward)
        self.msgHandler.put(SwReqMsg(switch=id, direction=direction))



			
