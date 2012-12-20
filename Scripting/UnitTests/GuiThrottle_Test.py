from GuiThrottle import GuiThrottle
from TCP import RailSocket
import StartAndKill as sak
import time
from Layout import readLayoutFile
from Log import openLog, closeLog, flushLog
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

    view = GuiThrottle(sk)
    view.mainloop()
        
    flushLog()
    input("press enter to quit")
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()
