#########################################################################################
########################### Unit Testing ################################################
#########################################################################################
"""
Before testing
   Comment out "import subprocess" at top of StartAndKill.py
After testing
   Comment out class subprocess at top of StartAndKill.py
"""
from Log import openLog, closeLog
from StartAndKill import start, kill, setTestingMode

if __name__ == "__main__":
    setTestingMode()
    openLog()
    start("simulator")
    start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
    start("ut4", ip = "127.0.0.1", port = "1234")
    start("RBLDisplay", ip = "127.0.0.1", port = "1235")
    start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    print ("\n")
    start("controller", trace = "no")
    start("ut4", port = "1236")
    start("RBLDisplay", ip = "127.1.1.1")
    start("adminthrottle",  ip = "127.0.0.1", port = "1234", logs = "yes")
    print ("\n")
    kill("sim")
    kill("cont")
    kill("ut4")
    kill("RBL")
    kill("admin")
    kill()
    kill("all")
    raw_input("press enter to quit")
    closeLog()
