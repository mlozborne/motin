##########################################################################################
import StartAndKill as sak
from MessageTranslationTypes import *
from Log import *
from time import sleep
from MsgHandler import *
from Throttle import Throttle
from multiprocessing import Queue

def stopTrain(self):
    self.setSpeed(0)

def blinkLights(self, n):
    for i in range(5):
        self.setLights(kOn)
        sleep(0.2)
        self.setLights(kOff)
        sleep(0.2)

def tootHorn(self):
    self.setHorn(kOn)
    sleep(1)

    self.setHorn(kOff)
    sleep(.2)

    self.setHorn(kOn)
    sleep(.2)

    self.setHorn(kOff)
    sleep(.2)

    self.setHorn(kOn)
    sleep(1)

    self.setHorn(kOff)

if __name__ == "__main__":
    openLog()

    #  Start the simulator and controller
    sak.start("simulator")
    sak.start("controller")
    
    # Set up the message pumps and a throttle
    outQu = Queue()
    msgPumpHandler = MsgPumpHandler(name = 'for Throttle Test', host = 'localhost', port = 1235, outQu = outQu)
    msgPumpHandler.startPumps()
    throt = Throttle(name = 'Bill', outQu = outQu)
    
    # Tell the throttle to read the layout file
    printLog("Main reading layout")
    msg = throt.readLayout("../../runSoftware/Layout.xml")
    sleep(2)

    # Tell the throttle to initialize train 1111
    printLog("Main initializing train")
    msg = throt.initTrain(1111, [5,1])
    printLog("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".
              format(msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot))
    if msg.physSlot > 120:
        print("\nABEND: couldn't initialize the train. Response code = {0}".format(msg.physSlot))
        input("press enter to quit")


    # Use the throttle to send messages to the controller
    throt.setBell(kOn)
    throt.do(blinkLights, 4)
    throt.setBell(kOff)
    throt.do(tootHorn)
    throt.setSpeed(100)
    sleep(3)
    throt.throwNextSwitch()
    throt.moveSwitch(12, kClosed)
    sleep(4)
    throt.moveSwitch(12, kThrown)

    # Stop the train when it reaches sensor 59
    throt.addInterest(PutSensorStateMsg)
    throt.waitFor(PutSensorStateMsg(id = 59, state = kSensorOpen))
    throt.removeInterest(PutSensorStateMsg)
    throt.do(stopTrain)
    flushLog()
    input("press enter to quit")
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()






