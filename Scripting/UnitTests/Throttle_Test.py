##########################################################################################
"""
Test TCP basics
"""
import StartAndKill as sak
from Layout import readLayoutFile
from TCP import *
from Throttle import *
from MessageTranslationTypes import kOn, kOff, kClosed, kThrown
from Log import *

def stopTrain(self):
    self.setSpeed(0)

def blinkLights(self, n):
    for i in range(5):
        self.setLights(kOn)
        time.sleep(0.2)
        self.setLights(kOff)
        time.sleep(0.2)

def tootHorn(self):
    self.setHorn(kOn)
    time.sleep(1)

    self.setHorn(kOff)
    time.sleep(.2)

    self.setHorn(kOn)
    time.sleep(.2)

    self.setHorn(kOff)
    time.sleep(.2)

    self.setHorn(kOn)
    time.sleep(1)

    self.setHorn(kOff)

if __name__ == "__main__":
    openLog()
    sak.start("simulator")
    sak.start("controller")
    time.sleep(3)
    sk = RailSocket('localhost', 1235)

    responseFlag, code = readLayoutFile(sk, b"../../runSoftware/Layout.xml")
    print("responseFlag = {0} and code ={1}".format(responseFlag, code))
    if responseFlag != 1:
        print("ABEND")
        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
        print ("THE END")
        input("press enter to quit")
        
    throt = Throttle(sk)
    physAdd, physSlot, virtAdd, virtSlot = throt.doLocoInit(1111, [5, 1])
    print("physAdd = {0}, physSlot = {1}, virtAdd = {2}, virtSlot = {3}".format(physAdd, physSlot, virtAdd, virtSlot))
    if physSlot > 120:
        print("\nABEND: couldn't initialize the train. Response code = {0}".format(physSlot))
        input("press enter to quit")
        
    throt.setBell(kOn)
    throt.do(blinkLights, 4)
    throt.setBell(kOff)
    throt.do(tootHorn)
    throt.setSpeed(100)
    time.sleep(3)
    throt.throwNextSwitch()
    throt.moveSwitch(12, kClosed)
    time.sleep(4)
    throt.moveSwitch(12, kThrown)
    time.sleep(5)
    throt.do(stopTrain)

    flushLog()
    input("press enter to quit")
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()






