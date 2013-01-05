from GuiThrottle import GuiThrottleProcess
#from TCP import RailSocket
import StartAndKill as sak
from time import sleep
from Log import openLog, closeLog, flushLog
from multiprocessing import Queue
from Throttle import Throttle
from MsgHandler import *
from MessageTranslationLibrary import PutInitOutcomeMsg, PutTrainPositionMsg, PutTrainStateMsg, PutReadLayoutResponseMsg

from GuiThrottle import GuiThrottle

if __name__ == "__main__":
    openLog()

    name = "Bill"

    #  Start the simulator and controller
    sak.start("simulator")
    sak.start("controller")

    # 1 outQu
    outQu = Queue()

    #5 inQus
    inQuList = []
    # 4 queues for 4 gui throttles
    for i in range(4):
        inQuList.append(InQuListEntry(inQu = Queue(), interests = [PutInitOutcomeMsg, PutTrainPositionMsg, PutTrainStateMsg]))
    # 1 queue for throttle
    inQuList.append(InQuListEntry(inQu = Queue(), interests = [PutReadLayoutResponseMsg]))

    # Start msgPumpHandler
    msgPumpHandler = MsgPumpHandler(name = name, host = 'localhost', port = 1235, inQuList = inQuList, outQu = outQu)
    msgPumpHandler.startPumps()

    # Start a throttle to read the layout file
    throttle = Throttle(name = name, inQu = inQuList[4].inQu, inQuNum = 4, outQu = outQu)
    printLog("Main: read layout file")
    msg = throttle.readLayout("../../runSoftware/Layout.xml")
    sleep(2)
    responseFlag, code = (msg.responseFlag, msg.code)
    printLog("Main: finished reading layout file: responseFlag = {0} and code ={1}".format(responseFlag, code))
    if responseFlag != 1:
        print("ABEND")
        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
        print ("THE END")
        input("press enter to quit")
    throttle.close()                              <<<<<<<<<<<<<<<<<<<   doesn't work unless commented out

    # Start four gui throttles and pass inQu and outQu
    printLog("Main: begin start four GuiThrottleProcess")
    for i in range(4):
        GuiThrottleProcess(name = str(i+1), inQu = inQuList[i].inQu, inQuNum = i, outQu = outQu).start()
        sleep(1)
    printLog("Main: end start four GuiThrottleProcess")
    flushLog()
    
    sleep(1)          ##### WEIRD If sleep is omitted, then the input statement
                      ##### blocks the gui from being displayed.

    input("press enter to quit")

    sak.kill("controller")
    sak.kill("simulator")
    closeLog()
