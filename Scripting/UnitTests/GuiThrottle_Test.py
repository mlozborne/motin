from GuiThrottle import GuiThrottleProcess
from TCP import RailSocket
import StartAndKill as sak
from time import sleep
from Log import openLog, closeLog, flushLog
from multiprocessing import Queue
from Throttle import Throttle
from MsgHandler import *


if __name__ == "__main__":
    openLog()

    #  Start the simulator and controller
#    sak.start("simulator")
#    sak.start("controller")
    
    # Connect a message socket to the controller
#    sk = MsgSocket()
#    sk.connect('localhost', 1235)
    
    # Create two queues and start two message pumps
    outQu = Queue()
    inQu = Queue()
#    MsgOutQuPump(sock = sk, qu = outQu).start()
#    MsgInQuPump(sock = sk, qu = inQu).start()
#
#    # Create a throttle
#    throttle = Throttle(nm = "1", inQu = inQu, outQu = outQu)
#
#    # Tell the throttle to read the layout file and check response
#    printLog("Main read layout file")
#    msg = throttle.readLayout("../../runSoftware/Layout.xml")
#    printLog("Main finished reading layout file")
#    sleep(2)
#    responseFlag, code = (msg.responseFlag, msg.code)
#    print("readLayoutFile: responseFlag = {0} and code ={1}".format(responseFlag, code))
#    if responseFlag != 1:
#        print("ABEND")
#        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
#        print ("THE END")
#        input("press enter to quit")

    # Create a GuiThrottleProcess and pass inQu and outQu
    printLog("Main initializing GuiThrottleProcess")
    gtp = GuiThrottleProcess(nm = "1", inQu = inQu, outQu = outQu)
    printLog("Main starting GuiThrottleProcess")
    gtp.start()
    flushLog()

    input("press enter to quit")
#    sk.close()
#    sak.kill("controller")
#    sak.kill("simulator")
#    closeLog()
