from Log import printLog
import platform
 
# Comment this out during testing
#import subprocess

# Comment this out during production
class Subprocess:
    def call(self, st, shell = ""):
        print st
subprocess = Subprocess()


gPath = "../../runSoftware"

def setPath(st):
    global gPath
    gPath = st
    printLog("StartAndKill: path is " + st)

def start(name, ip = "", port = "", trace = "", logs = "", layoutFile = ""):
    """
    Usage (only uses first three letters of name)
      start("simulator")
      start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
      start("ut4", ip = "127.0.0.1", port = "1234")
      start("RBLDisplay", ip = "127.0.0.1", port = "1235")
      start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    """
    # start("simulator")
    if name.lower()[:3] == "sim":
        startThis = "start " + gPath + "/RailroadBig.exe"
        subprocess.call(startThis, shell=True)

    # start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
    elif name.lower()[:3] == "con":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1234"    # default to the simulator
        if trace == "" or trace.lower()[:1] == "y":
            trace = "yes"   # default to trace on
        else:
            trace = "no"
        startThis = "start " + gPath + "/StartController.exe" + \
            " IP " + ip + \
            " PORT " + port + \
            " TRACE " + trace
        subprocess.call(startThis, shell=True)

    # start("ut4", ip = "127.0.0.1", port = "1234")
    elif name.lower()[:3] == "ut4":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1234"    # default to the simulator
        startThis = "start " + gPath + "/Throttle.exe" + \
            " IP " + ip + \
            " PORT " + port
        subprocess.call(startThis, shell=True)

    # start("RBLDisplay", ip = "127.0.0.1", port = "1235")
    elif name.lower()[:3] == "rbl":
        if ip == "": ip = "127.0.0.1"   # default to local host
        if port == "": port = "1235"    # default to the controller
        startThis = "start " + gPath + "/RBLDisplay.exe" + \
            " IP " + ip + \
            " PORT " + port
        subprocess.call(startThis, shell=True)

    # start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
    elif name.lower()[:3] == "adm":
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
        startThis = "start " + gPath + "/adminthrottle.exe" + \
            " IP " + ip + \
            " PORT " + port + \
            " MODE " + mode + \
            " KEYBOARDLOG " + logs + \
            " ADMINLOG " + logs
        subprocess.call(startThis , shell=True)
        
    # Log it
    printLog("StartAndKill: " + startThis)

def kill(name = "all"):
    """
    Usage (only uses first three letters of name)
    If name unrecognized, then defaults to "all"
      kill("simulator")
      kill("controller")
      kill("ut4")
      kill("RBLDisplay")
      kill("adminthrottle")
      kill("all")
    """
    if   name.lower()[:3] == "sim": name = "RailroadBig"
    elif name.lower()[:3] == "con": name = "StartController"
    elif name.lower()[:3] == "ut4": name = "Throttle"
    elif name.lower()[:3] == "rbl": name = "RBLDisplay"
    elif name.lower()[:3] == "adm": name = "adminthrottle"
    elif name.lower()[:3] == "all": name = "all"
    else                          : name = "all"
    if name != "all":
        if "XP" in platform.platform():
            killThis = "tskill " + name 
            subprocess.call(killThis, shell=True)
        else:
            killThis = "taskkill /T /IM " + name
            subprocess.call(killThis, shell=True)
        printLog("StartAndKill: " + killThis)
    else:
        if "XP" in platform.platform():
            subprocess.call("tskill RailroadBig", shell=True)
            subprocess.call("tskill StartController", shell=True)
            subprocess.call("tskill Throttle", shell=True)
            subprocess.call("tskill RBLDisplay", shell=True)
            subprocess.call("tskill adminthrottle", shell=True)
            printLog("StartAndKill: tskill RailroadBig, StartController, Throttle, RBLDisplay, AdminThrottle")
        else:
            subprocess.call("taskkill /T /IM RailroadBig", shell=True)
            subprocess.call("taskkill /T /IM StartController", shell=True)
            subprocess.call("taskkill /T /IM Throttle", shell=True)
            subprocess.call("taskkill /T /IM RBLDisplay", shell=True)
            subprocess.call("taskkill /T /IM adminthrottle", shell=True)
            printLog("StartAndKill: taskkill /T /IM RailroadBig, StartController, Throttle, RBLDisplay, AdminThrottle")

    
