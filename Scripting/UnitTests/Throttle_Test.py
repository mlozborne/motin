##########################################################################################
"""
Test TCP basics
"""
import StartAndKill as sak
from Layout import readLayout
from TCP import *
from Train import *

def stopTrain(self):
    self.setSpeed(0)

def blink(self, n):
    for i in range(5):
        self.setLights(kOn)
        time.sleep(1)
        self.setLights(kOff)
        time.sleep(1)

if __name__ == "__main__":
    start("simulator")
    start("controller")
    time.sleep(3)
    sk = RailSocket('localhost', 1235)

    responseFlag, code = readLayoutFile(sk, "../../runSoftware/Layout.xml")
    print "responseFlag = {0} and code ={1}".format(responseFlag, code)
    if responseFlag != 1:
        print "ABEND"
        print "Error in XML file with flag = {0} and code = {1}".format(responseFlag, code)
        print "THE END"
        raw_input("press enter to quit")
        return

    throt = Throttle(sk)
    physAdd, physSlot, virtAdd, virtSlot = throt.doLocoInit(1111, [5, 1])
    if physSlot > 120:
        print "\nABEND: couldn't initialize the train. Response code = {0}".format(physSlot)
        return
    throt.do(blink, 4)
    throt.setSpeed(100)
    throt.throwNextSwitch()
    throt.moveSwitch(4, kClosed)
    time.sleep(2)
    throt.moveSwitch(4, kThrown)
    throt.do(stopTrain)

    raw_input(press enter to quit)
    sk.close()
    kill("controller")
    kill("simulator")



#########################################################################
################  Unit Testing                  #########################
#########################################################################

from functools import partial
import time





