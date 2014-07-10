from Log import gLog
from MessageTranslationTypes import *
from MessageTranslationLibrary import splitPutTrainPositionStr
from MsgHandler import CommunicationsPackage, MsgHandler
import multiprocessing
from multiprocessing.queues import Queue
from time import sleep

"""
In the following lists or tuples are okay.

COMMANDS and PATHS
-----------------------------
command
  [method, arg1, arg2, ...]
  [initTrain, 1111, [1, 5]]
  [setSpeed, 100]
  [closeNextSwitch]

commands
  [command, command, ...]
  [[initTrain, 1111, [1, 5]], [setSpeed, 100], [closeNextSwitch]]

commandPath
  [[sensor#, command], [sensor#, command], ...]
  ((6, (moveSwitch, 4, kThrown)), (6, (moveSwitch, 12, kThrown)), (94, (setDirection, kBackward)), (2, (stopTrain,)))

switchPath
  [[sensor#, switch#, direction], [sensor#, switch#, direction], ...]
  ((77, 18, kThrown), (77, 22, kClosed), (77, 15, kThrown), (80, 17, kClosed))

sensorPath
  [sensor#, sensor#, ...], where the numbers are topologically consecutive

sensorsToExclude
  [sensor#, sensor#, ...], where the numbers do NOT need to be topologically consecutive

DOS and FOLLOWS
---------------
  throttle.do(method, arg1, arg2, ...)
  doCommands(throttle, commands)
  followCommandPath(throttle, commands)
  followSensorPath(throttle, sensorPath)
  follosSwitchPath(throttle, switchPath)
"""

###################################################################################################
"""
This section repeats all the Throttle methods in a form that can be used in command lists.
"""
def doCommands(throttle, commands):
    for command in commands:
        if len(command) == 1:
            throttle.do(command[0])
        elif len(command) == 2:
            throttle.do(command[0], command[1])
        elif len(command) == 3:
            throttle.do(command[0], command[1], command[2])
        elif len(command) == 3:
            throttle.do(command[0], command[1], command[2], command[3])

# def atSpeedGoTo(self, speed, destination, sensorsToExclude):
#     self.atSpeedGoTo(speed, destination, sensorsToExclude)

def atSpeedGoTo(self, speed, destination, sensorsToExclude):
    return

# def followCommandPath(self, commandPath):
#     self.followCommandPath(commandPath)

def followCommandPath(self, commandPath):
    return

# def followSwitchPath(self, path):
#     self.followSwitchPath(path)

def followSwitchPath(self, path):
    return

# def followSensorPath(self, path):
#     self.followSensorPath(path)

def followSensorPath(self, path):
    return

# def addInterest(self, interest):
#     self.addInterest(interest)

def addInterest(self, interest):
    return

# def atSensorDo(self, sensor, command):
#     self.atSensorDo(sensor, command)

def atSensorDo(self, sensor, command):
    return

# def removeInterest(self, interest):
#     self.removeInterest(interest)

def removeInterest(self, interest):
    return

# def waitFor(self, msg):
#     self.waitFor(msg)

def waitFor(self, msg):
    return

def initTrain(self, address, position):
    self.initTrain(address, position)

# def initTrain(self, address, position):
#     return

def setSpeed(self, speed):
    self.setSpeed(speed)

# def setSpeed(self, speed):
#     return

# def setDirection(self, direction):
#     self.setDirection(direction)

def setDirection(self, direction):
    return

def setLights(self, onOff):
    self.setLights(onOff)

# def setLights(self, onOff):
#     return

# def setHorn(self, onOff):
#     self.setHorn(onOff)

def setHorn(self, onOff):
    return

# def setBell(self, onOff):
#     self.setBell(onOff)

def setBell(self, onOff):
    return

# def setMute(self, onOff):
#     self.setMute(onOff)

def setMute(self, onOff):
    return

def closeNextSwitch(self):
    self.closeNextSwitch()

# def closeNextSwitch(self):
#     return

def throwNextSwitch(self):
    self.throwNextSwitch()

# def throwNextSwitch(self):
#     return

def moveSwitch(self, sId, direction):
    self.moveSwitch(sId, direction)

# def moveSwitch(self, sId, direction):
#     return

# def pause(self, secs):
#     self.pause(secs)

def pause(self, secs):
    return

# def makeSectionUsable(self, sensor1, sensor2):
#     self.makeSectionUsable(sensor1, sensor2)

def makeSectionUsable(self, sensor1, sensor2):
    return

# def getPath(self, pathKind, preSensor, fromSensor, toSensor, sensorsToExclude):
#     return self.getPath(pathKind, preSensor, fromSensor, toSensor, sensorsToExclude)

def getPath(self, pathKind, preSensor, fromSensor, toSensor, sensorsToExclude):
    return

####################################################################################################
class Throttle(object):
    def __init__(self, name = None, comPkg = None):
        gLog.print("Throttle {0}: initializing".format(name))

        assert(isinstance(name, str))

        assert(isinstance(comPkg, CommunicationsPackage))
        inQu = comPkg.inQu
        inQuNum = comPkg.inQuNum
        outQu = comPkg.outQu

        assert(isinstance(inQu, multiprocessing.queues.Queue))
        assert(inQuNum >= 0)
        assert(isinstance(outQu, multiprocessing.queues.Queue))

        self.name = name
        self.virtSlot = None
        self.direction = kForward
        self.lights = kOff
        self.horn = kOff
        self.bell = kOff
        self.mute = kOff
        self.F5 = 0              # Flip this to close next turnout
        self.F6 = 0              # Flip this ot throw next turnout
        self.msgHandler = MsgHandler(name = self.name, comPkg = comPkg)

    def addInterest(self, interest):
        gLog.print("Throttle {0}: adding interest {1}".format(self.name, interest))
        assert(interest in ControllerInMsgs)
        self.msgHandler.addInterest(interest)

    def removeInterest(self, interest):
        gLog.print("Throttle {0}: removing interest {1}".format(self.name, interest))
        assert(interest in ControllerInMsgs)
        self.msgHandler.removeInterest(interest)

    def waitFor(self, msg):
        gLog.print("waitFor: msg = {0}".format(msg))
        assert(isinstance(msg, tuple))
        return self.msgHandler.waitFor(msg)

    def close(self):
        gLog.print("Throttle {0}: closing".format(self.name))
        self.msgHandler.close()

    def getBlocking(self):
        msg = self.msgHandler.getBlocking()
        #gLog.print("Throttle {0}: getBlocking msg {1}".format(self.name, msg))
        return msg

    def getNonblocking(self):
        try:
            msg = self.msgHandler.getNonblocking()
            #gLog.print("Throttle {0}: getNonlocking msg {1}".format(self.name, msg))
            return msg
        except multiprocessing.queues.Empty:
            gLog.print("Throttle {0}: getNonlocking empty qu exception".format(self.name))
            raise multiprocessing.queues.Empty

    def put(self, msg):
        self.msgHandler.put(msg)

    def atSpeedGoTo(self, speed, destination, sensorsToExclude):
        self.addInterest(PutTrainPositionMsg)
        self.put(GetTrainPositionMsg(slot=self.virtSlot))
        done = False
        while not done:
            msg = self.waitFor(PutTrainPositionMsg(slot=self.virtSlot, sensors=[]))
            if msg.slot == self.virtSlot:
                self.removeInterest(PutTrainPositionMsg)
                done = True
        fromSensor = msg.sensors[0]
        preSensor = msg.sensors[1]
        path = self.getPath(kBreadthFirst, preSensor, fromSensor, destination, sensorsToExclude)
        self.setSpeed(speed)
        self.followSensorPath(path)

    def followCommandPath(self, path):
        """
        A command path is a list of tuples (sensor#, command)
        When the train reaches the sensor, the throttle executes the command.
        """
        self.addInterest(PutSensorStateMsg)
        previousSensor = 0
        for point in path:
            sensor = point[0]
            command = point[1]
            if sensor != previousSensor:
                previousSensor = sensor
                self.waitFor(PutSensorStateMsg(id = sensor, state = kSensorOpen))
            if len(command) == 1:
                self.do(command[0])
            if len(command) == 2:
                self.do(command[0], command[1])
            if len(command) == 3:
                self.do(command[0], command[1], command[2])
        self.removeInterest(PutSensorStateMsg)

    def atSensorDo(self, sensor, command):
        commandPath = [(sensor, (command,))]
        followCommandPath(self, commandPath)

    def followSwitchPath(self, path):
        """
        A switch path is a list of triples (sensor#, switch#, direction)
        When the train reaches the sensor, the throttle moves the switch.
        """
        self.addInterest(PutSensorStateMsg)
        previousSensor = 0
        for point in path:
            sensor = point[0]
            switch = point[1]
            direction = point[2]
            if sensor != previousSensor:
                previousSensor = sensor
                self.waitFor(PutSensorStateMsg(id = sensor, state = kSensorOpen))
            self.moveSwitch(switch, direction)
        self.removeInterest(PutSensorStateMsg)

    def followSensorPath(self, path):
        """
        A sensor path is a list or tuple of sensor numbers defining the path which a train
        desires to follow. As the train approaches section (i, i+1),  the throttle makes section (i+1, i+2)
        usable. To get things started, the throttle makes the section (0, 1) usable,
        where i indicates sensor i.
        """
        self.addInterest(PutSensorStateMsg)
        self.makeSectionUsable(path[0], path[1])
        for i in range(0,len(path)-2):
            self.waitFor(PutSensorStateMsg(id = path[i], state = kSensorOpen))
            self.makeSectionUsable(path[i+1], path[i+2])
        self.removeInterest(PutSensorStateMsg)

################################################################################

    def readLayout(self, fileName):
        gLog.print("Throttle {0}: sending DoReadLayoutMsg using file {1}".format(self.name, fileName))
        assert(isinstance(fileName, str))
        self.addInterest(PutReadLayoutResponseMsg)
        self.put(DoReadLayoutMsg(fileName = fileName))
        msg = self.waitFor(PutReadLayoutResponseMsg(responseFlag = 0, code = 0))
        self.removeInterest(PutReadLayoutResponseMsg)
        sleep(3)
        return msg

    def initTrain(self, address, position):
        gLog.print("Throttle {0}: sending DoLocoInitMsg".format(self.name))
        assert(0 <= address <= 9999)
        assert(isinstance(position, list) or isinstance(position, tuple))
        self.put(DoLocoInitMsg(address = address, sensors = position))
        responseMsg = None
        if position:
            self.addInterest(PutInitOutcomeMsg)
            responseMsg = self.waitFor(PutInitOutcomeMsg(physAdd = address, physSlot = 0, virtAdd = 0, virtSlot = 0))
            self.removeInterest(PutInitOutcomeMsg)
            if responseMsg.physSlot > 120:
                self.virtSlot = None
            else:
                self.virtSlot = responseMsg.virtSlot
        return responseMsg

    def getPath(self, pathKind, preSensor, fromSensor, toSensor, sensorsToExclude):
        gLog.print("Throttle {0}: sending GetPathMsg".format(self.name))
        self.addInterest(PutPathMsg)
        self.put(GetPathMsg(slot=self.virtSlot, pathKind=pathKind,
                 preSensor=preSensor, fromSensor=fromSensor, toSensor=toSensor,
                 sensorsToExclude = sensorsToExclude))
        msg = self.waitFor(PutPathMsg(sensors=[]))
        self.removeInterest(PutPathMsg)
        return msg.sensors

    def setVirtSlot(self, virtSlot):
        self.virtSlot = virtSlot

    def do(self, func, * args):
        func(self, * args)

    def doCommands(self, commands):
        for command in commands:
            if len(command) == 1:
                self.do(command[0])
            elif len(command) == 2:
                self.do(command[0], command[1])
            elif len(command) == 3:
                self.do(command[0], command[1], command[2])
            elif len(command) == 3:
                self.do(command[0], command[1], command[2], command[3])

    def sendDirf(self):
        assert(self.virtSlot is not None)
        self.put(LocoDirfMsg(slot=self.virtSlot, direction=self.direction,
                            lights=self.lights, horn=self.horn, bell=self.bell))

    def sendSnd(self):
        assert(self.virtSlot is not None)
        self.put(LocoSndMsg(slot=self.virtSlot, mute=self.mute, F5=self.F5, F6=self.F6))

    def setSpeed(self, speed):
        assert(self.virtSlot is not None)
        assert(speed >= 0)
        self.put(LocoSpdMsg(slot=self.virtSlot, speed=speed))

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

    def moveSwitch(self, sId, direction):
        assert(sId > 0)
        assert(direction == kForward or direction == kBackward)
        self.put(SwReqMsg(switch=sId, direction=direction))

    def pause(self, secs):
        sleep(secs)

    def makeSectionUsable(self, sensor1, sensor2):
        assert(sensor1 > 0)
        assert(sensor2 > 0)
        self.put(DoMakeSectionUsableMsg(sensor1=sensor1, sensor2=sensor2))
