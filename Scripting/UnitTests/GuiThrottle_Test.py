from GuiThrottle import ThrottleProcess
from TCP import RailSocket
import StartAndKill as sak
import time
from Layout import readLayoutFile
from Log import openLog, closeLog, flushLog
from multiprocessing import Queue

if __name__ == "__main__":
    openLog()
    sak.start("simulator")
    sak.start("controller")
    time.sleep(3)
    sk = RailSocket('localhost', 1235)

    responseFlag, code = readLayoutFile(sk, b"../../runSoftware/Layout.xml")
    print("readLayoutFile: responseFlag = {0} and code ={1}".format(responseFlag, code))
    if responseFlag != 1:
        print("ABEND")
        print("Error in XML file with flag = {0} and code = {1}".format(responseFlag, code))
        print ("THE END")
        input("press enter to quit")

    qu = Queue()
    ThrottleProcess(sk, qu).start()
    while True:
        msg = sk.receive()
        qu.put(msg)
        
    flushLog()
    input("press enter to quit")
    sk.close()
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()
