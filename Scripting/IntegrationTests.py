##########################################################################################
"""
Test TCP basics
"""
import subprocess
from threading import Thread

from MessageTranslationLibrary import *
from TCP import *
		
########################################################################

from Train import *
from Layout import *

def stopTrain(self):
    self.setSpeed(0)
    self.setLights(kOff)
	
def blink(self, n):
    print "\nIn blink n = {0}\n".format(n)
    for i in range(0, 3):
        self.setLights(kOn)
        time.sleep(1)
        self.setLights(kOff)
        time.sleep(1)

def testControllingTrain():
    print "Starting simulator and controller"
    subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)
    time.sleep(1)
    subprocess.call("start ../runSoftware/StartController.exe IP 127.0.0.1 PORT 1234 TRACE yes", shell=True)
    #subprocess.call("start ../runSoftware/AdminThrottle.exe IP 127.0.0.1 PORT 1235 MODE controller LAYOUTFILE ../runSoftware/Layout.xml KEYBOARDLOG no ADMINLOG yes", shell=True)
    time.sleep(3)
	
    print "\nConnect a socket to the controller"
    sk = RailSocket('localhost', 1235)
	
    print "\nRead the layout file"
    responseFlag, code = readLayoutFile(sk, "../runSoftware/Layout.xml")
    print "responseFlag = {0} and code ={1}".format(responseFlag, code)
    if responseFlag != 1:
        print "ABEND"
        print "Error in XML file with flag = " + repr(responseFlag) + " and code = " + repr(code)
        print "THE END"
        return	
	
    print "\nCommand the train"
    tr = Train(1111, sk)
    response = tr.doLocoInit([5, 1])
    if response > 120:
        print "\nABEND: couldn't initialize the train. Response code = {0}".format(response)
        return		
    tr.do(blink, 4)
    tr.setSpeed(100)	
    sk.send(SwReqMsg(switch=4, direction=kThrown))
    tr.waitForSensor(62)
    tr.do(stopTrain)
	
    time.sleep(1)

#################################################################
if __name__ == "___main__":
    x = 1
    #testControllingTrain()
    #testTalkingToController()
    #testReceive()

