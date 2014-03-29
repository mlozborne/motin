from GuiThrottle import GuiThrottleProcess
from Log import closeLog, flushLog, openLog, printLog
from MsgHandler import *
import StartAndKill as sak
from Throttle import Throttle
from time import sleep

#def raw_input(str):
#    return input(str)

###################################################

if __name__ == "__main__":
    openLog()

    name = "Bill"

    #  Start the simulator and controller
    sak.start("simulator")
    sak.start("controller")

    # Create the communication resources for 5 users
    comRes = CommunicationResources(host = 'localhost', port = 1235, numberOfPackages = 5)

    # Create a throttle to read the layout file
    throttle = Throttle(name = name, comPkg = comRes.getNextPackage())
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
    throttle.close()

    # Start four gui throttles 
    printLog("Main: begin start four GuiThrottleProcess")
    for i in range(4):
        GuiThrottleProcess(name = str(i+1), comPkg = comRes.getNextPackage()).start()
        sleep(1)
    printLog("Main: end start four GuiThrottleProcess")
    flushLog()

    sleep(1)          ##### WEIRD If sleep is omitted, then the input statement
                      ##### blocks the gui from being displayed.

    raw_input("press enter to quit")

    sak.kill("controller")
    sak.kill("simulator")
    closeLog()

