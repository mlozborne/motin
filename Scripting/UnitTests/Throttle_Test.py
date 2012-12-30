##########################################################################################
import StartAndKill as sak
from MessageTranslationTypes import *
from Log import *
from time import sleep
from MsgHandler import MsgSocket, MsgInQuPump, MsgOutQuPump, waitFor
from Throttle import Throttle
from threading import Thread
from multiprocessing import Queue

class MsgQuPump(Thread):
    def __init__(self, name = "1", sock = None, qu = None):
        printLog("Creating MessageQueuePump {0}".format(id))
        Thread.__init__(self)
        self.name = name
        self.sk = sock
        self.qu = qu

    def run(self):
        printLog("Starting MessageQueuePump {0}".format(self.name))
        while True:
            sk.send(self.qu.get())

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
    
    # Connect a message socket to the controller
    sk = MsgSocket()
    sk.connect('localhost', 1235)
    
    # Create two queues and start two message pumps
    outQu = Queue()
    inQu = Queue()
    inQuList = []
    inQuList.append(inQu)
    MsgOutQuPump(sock = sk, qu = outQu).start()
    MsgInQuPump(sock = sk, inQuList = inQuList).start()

    # Create a throttle       
    throt = Throttle(name = "1", inQu = inQu, outQu = outQu)

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
    waitFor(inQu, PutSensorStateMsg(id = 59, state = kSensorOpen))
    throt.do(stopTrain)
    flushLog()
    input("press enter to quit")
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()






