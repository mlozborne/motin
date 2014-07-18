from GuiThrottle import GuiThrottleProcess
from Log import gLog
from MsgHandler import *
from StartAndKill import StartAndKill
from Throttle import Throttle
from time import sleep


def raw_input(st):
    return input(st)

###################################################

if __name__ == "__main__":
    gLog.open()

    name = "Bill"

    #  Start the simulator and controller
    sak = StartAndKill()
    sak.start("simulator")
    sak.start("controller")

    # Create the communication resources for 5 users
    comRes = CommunicationResources(name = 'gui thr test', host = 'localhost', port = 1235, numberOfPackages = 5)

    # Create a throttle to read the layout file
    throttle = Throttle(name = name, comPkg = comRes.getNextPackage())
    gLog.print("Main: read layout file")
    msg = throttle.readLayout("../../runSoftware/Layout.xml")
    sleep(2)
    responseFlag, code = (msg.responseFlag, msg.code)
    gLog.print("Main: finished reading layout file: responseFlag = {0} and code ={1}".format(responseFlag, code))
    if responseFlag != 1:
        print("ABEND")
        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
        print("THE END")
        input("press enter to quit")
    throttle.close()

    # Start four gui throttles
    gLog.print("Main: begin start four GuiThrottleProcess")
    for i in range(4):
        GuiThrottleProcess(name = str(i+1), comPkg = comRes.getNextPackage()).start()
        sleep(1)
    gLog.print("Main: end start four GuiThrottleProcess")
    gLog.flush()

    sleep(1)          ##### WEIRD If sleep is omitted, then the input statement
                      ##### blocks the gui from being displayed.

    raw_input("press enter to quit\n")

    sak.kill("controller")
    sak.kill("simulator")
    gLog.close()

