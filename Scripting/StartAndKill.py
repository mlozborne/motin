import platform

# Comment this out during testing
#import subprocess    

# Comment this out during production
class Subprocess: 
    def call(self, st, shell = ""):
        print st
subprocess = Subprocess()


gPath = "../runSoftware"

def setPath(st):
    global gPath
    gPath = st

def start(name, ip = "", port = "", trace = "", logs = "", layoutFile = ""):
    """
    Usage
      start("simulator")
      start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
      start("ut4", ip = "127.0.0.1", port = "1234")
      start("RBLDisplay", ip = "127.0.0.1", port = "1235")
      start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    """
    # start("simulator")
    if name.lower()[:2] == "si":
        subprocess.call( \
            "start " + gPath + "/RailroadBig.exe", shell=True)

    # start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
    elif name.lower()[:2] == "co":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1234"    # default to the simulator
        if trace == "" or trace.lower()[:1] == "y":
            trace = "yes"   # default to trace on
        else:
            trace = "no"
        subprocess.call( \
            "start " + gPath + "/StartController.exe" + \
            " IP " + ip + \
            " PORT " + port + \
            " TRACE " + trace, shell=True)

    # start("ut4", ip = "127.0.0.1", port = "1234")
    elif name.lower()[:2] == "ut":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1234"    # default to the simulator
        subprocess.call( \
            "start " + gPath + "/Throttle.exe" + \
            " IP " + ip + \
            " PORT " + port , shell=True)

    # start("RBLDisplay", ip = "127.0.0.1", port = "1235")
    elif name.lower()[:2] == "rb":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1235"    # default to the controller
        subprocess.call( \
            "start " + gPath + "/RBLDisplay.exe" + \
            " IP " + ip + \
            " PORT " + port , shell=True)

    # start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    elif name.lower()[:2] == "ad":
        if ip == "": ip = "127.0.0.1"                           # default to local host
        if port == "" or port == "1235":
            port = "1235"                                       # default to controller
            mode = "controller"                                 # default to controller mode
        else:
            mode = "standalone"                                 # default to standalone mode
        if layoutFile == "": layoutFile = "layout.xml"  # default to layout.xml
        if logs == "" or logs.lower()[:1] == "n":
            logs = "no"                                         # default to logs off
        else:
            logs = "yes"
        subprocess.call( \
            "start " + gPath + "/adminthrottle.exe" + \
            " IP " + ip + \
            " PORT " + port + \
            " MODE " + mode + \
            " KEYBOARDLOG " + logs + \
            " ADMINLOG " + logs, shell=True)

def kill(name):
    if   name.lower()[:2] == "si": name = "RailroadBig"
    elif name.lower()[:2] == "co": name = "StartController"
    elif name.lower()[:2] == "ut": name = "Throttle"
    elif name.lower()[:2] == "rb": name = "RBLDisplay"
    elif name.lower()[:2] == "ad": name = "adminthrottle"
    if "XP" in platform.platform():
        subprocess.call("tskill " + name + ".exe", shell=True)
    else:
        subprocess.call("taskkill /T /IM " + name + ".exe", shell=True)



#########################################################################################
########################### Unit Testing ################################################
#########################################################################################

"""
Before testing
   Comment out "import subprocess" at top of this file.
After testing
   Comment out class subprocess at top of this file
"""

if __name__ == "__main__":
    start("simulator")
    start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
    start("ut4", ip = "127.0.0.1", port = "1234")
    start("RBLDisplay", ip = "127.0.0.1", port = "1235")
    start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    print "\n"
    start("controller", trace = "no")
    start("ut4", port = "1236")
    start("RBLDisplay", ip = "127.1.1.1")
    start("adminthrottle",  ip = "127.0.0.1", port = "1234", logs = "yes")
    print "\n"
    kill("sim")
    kill("cont")
    kill("ut4")
    kill("RBL")
    kill("admin")
    raw_input("...")
    
