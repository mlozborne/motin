import time
from Log import printLog
from MessageTranslationLibrary import DoReadLayoutMsg, PutReadLayoutResponseMsg
from TCP import *

#class Sensor(object):
#    def __init__(self):
#        self.state =

def readLayoutFile(sk, fileName):
    """
    Won't work because shouldn't receive directly  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    """
    printLog("Layout: reading file = {0}".format(fileName))
    time.sleep(2)
    sk.send(DoReadLayoutMsg(fileName=fileName))
    msg = sk.receive()
    while not isinstance(msg, PutReadLayoutResponseMsg):
        msg = sk.receive()
    time.sleep(3)
    return msg.responseFlag, msg.code



#    def updateFromMessage(self, msg):
#        if isinstance(msg, PutTrainStateMsg) and msg.slot == self.slot:
#            self.state = msg.state
#        elif isinstance(msg, PutTrainPositionMsg) and msg.slot == self.slot:
#            self.sensors = msg.sensors
#        elif isinstance(msg, PutTrainInformationMsg) and msg.slot == self.slot:
#            self.speed = msg.speed
#            self.direction = msg.direction
#            self.lights = msg.lights
#            self.bell = msg.bell
#            self.horn = msg.horn
#            self.mute = msg.mute
#
#    def waitForSensor(self, id):
#        while True:
#            msg = self.sk.receive()
#            if isinstance(msg, PutTrainPositionMsg):
#                if self.slot == msg.slot and id in (msg.sensors)[1:]:
#                    return
