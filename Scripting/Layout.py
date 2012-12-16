import time
from Log import printLog
from MessageTranslationLibrary import DoReadLayoutMsg, PutReadLayoutResponseMsg
from TCP import *

def readLayoutFile(sk, fileName):
    printLog("Layout: reading file = {0}".format(fileName))
    time.sleep(2)
    sk.send(DoReadLayoutMsg(fileName=fileName))
    msg = sk.receive()
    while not isinstance(msg, PutReadLayoutResponseMsg):
        msg = sk.receive()
    time.sleep(3)
    return msg.responseFlag, msg.code
