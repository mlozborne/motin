from GuiThrottle import GuiThrottleProcess
#from TCP import RailSocket
import StartAndKill as sak
from time import sleep
from Log import openLog, closeLog, flushLog
from multiprocessing import Queue
from Throttle import Throttle
from MsgHandler import *
from MessageTranslationLibrary import PutInitOutcomeMsg, PutTrainPositionMsg, PutTrainStateMsg, PutReadLayoutResponseMsg

#from GuiThrottle import GuiThrottle

if __name__ == "__main__":
    openLog("main")

    #  Start the simulator and controller
    sak.start("simulator")
    sak.start("controller")
    
    # Connect a message socket to the controller
    sk = MsgSocket()
    sk.connect('localhost', 1235)
    
    # Create out queue
    outQu = Queue()

    # Create in queues
    inQuList = []
    for i in range(4):               # 4 queues for 4 gui throttles
        inQuList.append(InQuListEntry(qu = Queue(), msgTypes = (PutInitOutcomeMsg, PutTrainPositionMsg, PutTrainStateMsg)))
    inQuList.append(InQuListEntry(qu = Queue(), msgTypes = (PutReadLayoutResponseMsg,)))  # 1 queue for throttle

    # Start the message pumps
    MsgOutQuPump(sock = sk, qu = outQu).start()
    MsgInQuPump(sock = sk, inQuList = inQuList).start()

    # Start a throttle to read the layout file
    throttle = Throttle(name = "1", inQu = inQuList[4].qu, outQu = outQu)
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

    # Start four gui throttles and pass inQu and outQu
    printLog("Main: begin start four GuiThrottleProcess")
    for i in range(4):
        GuiThrottleProcess(name = str(i+1), inQu = inQuList[i].qu, outQu = outQu).start()
        sleep(1)
    printLog("Main: end start four GuiThrottleProcess")
    flushLog()
    sleep(1)          ##### WEIRD If sleep is omitted, then the input statement
                      ##### blocks the gui from being displayed.
    input("press enter to quit")
    
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()
