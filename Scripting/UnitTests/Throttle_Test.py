##########################################################################################
import StartAndKill as sak
from Layout import readLayoutFile
from MessageTranslationTypes import kOn, kOff, kClosed, kThrown
from MessageTranslationLibrary import PutInitOutcomeMsg, DoLocoInitMsg, DoReadLayoutMsg, PutReadLayoutResponseMsg
from Log import *
from time import sleep
from  MsgSocket import MsgSocket
from Throttle import Throttle
from threading import Thread
from queue import Queue

class MessageQueuePump(Thread):
    def __init__(self, sock, qu):
        Thread.__init__(self)
        self.sk = sock
        self.qu = qu

    def run(self):
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
    openLog("", 1)

    #  Start the simulator and controller
    sak.start("simulator")
    sak.start("controller")
    sleep(3)

    # Connect a socket to the controller
    sk = MsgSocket()
    sk.connect('localhost', 1235)

    # Tell controller to read the layout file
    printLog("Reading layout file = ../../runSoftware/Layout.xml")
    sleep(2)
    sk.send(DoReadLayoutMsg(fileName= b"../../runSoftware/Layout.xml"))
    msg = sk.receive()
    while not isinstance(msg, PutReadLayoutResponseMsg):
        msg = sk.receive()
    sleep(3)
    responseFlag, code = msg.responseFlag, msg.code
    print("responseFlag = {0} and code ={1}".format(responseFlag, code))
    if responseFlag != 1:
        print("ABEND")
        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
        print ("THE END")
        input("press enter to quit")

    # Tell controller to initialize train 1111 at position (5,1)
    sk.send(DoLocoInitMsg(address=1111, sensors=[5,1]))
    msg = sk.receive()
    while not isinstance(msg, PutInitOutcomeMsg):
        msg = sk.receive()
    physAdd, physSlot, virtAdd, virtSlot = msg.physAdd, msg.physSlot, msg.virtAdd, msg.virtSlot
    print("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".format(physAdd, physSlot, virtAdd, virtSlot))
    if physSlot > 120:
        print("\nABEND: couldn't initialize the train. Response code = {0}".format(physSlot))
        input("press enter to quit")

    # Create the throttle-to-controller message queue
    qu = Queue()

    # Create the throttle
    throt = Throttle(qu)

    # Create a thread that will send messages from the queue to the controller
    MessageQueuePump(sk, qu).start

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
    sleep(5)
    throt.do(stopTrain)

    flushLog()
    input("press enter to quit")
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()






