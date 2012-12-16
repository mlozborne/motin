from Layout import readLayoutFile
from TCP import *
from Log import *
import StartAndKill as sak

if __name__ == "__main__":
    openLog()
    sak.start("simulator")
    sak.start("controller")
    sk = RailSocket('localhost', 1235)
    responseFlag, code = readLayoutFile(sk, "../../runSoftware/Layout.xml")
    if responseFlag  == 1:
        print "test passed"
    else:
        print "test failed with responseFlag = {0} and code = {1}".format(responseFlag, code)
    flushLog()
    raw_input("press enter to quit")    
    sak.kill("controller")
    sak.kill("simulator")
    closeLog()


