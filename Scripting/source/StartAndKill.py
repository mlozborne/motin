import platform
from time import sleep
from Log import gLog

import subprocess  # This gives access to shell commands via the "call" function

#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
# This class and the subsequent function are used to shadow out the
# imported "subprocess" during unit testing


class Subprocess:
    # noinspection PyUnusedLocal
    @staticmethod
    def call(st, shell=True):
        print(st)


def setTestingMode():  # the unit test needs to call this function
    global subprocess
    subprocess = Subprocess()


#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


class StartAndKill():
    def __init__(self, mode=''):
        self.sakPath = "../../runSoftware"
        if mode == 'testing':
            setTestingMode()

    def setPath(self, st):
        self.sakPath = st
        gLog.print("sak: path is {0}  ".format(st))

    def start(self, name, ip="", port="", trace="", logs="", layoutFile=""):
        """
        Usage (only uses first three letters of name)
          start("simulator")
          start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
          start("ut4", ip = "127.0.0.1", port = "1234")
          start("RBLDisplay", ip = "127.0.0.1", port = "1235")
          start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
        """
        startThis = ""
        # start("simulator")
        if name.lower()[:3] == "sim":
            startThis = "start " + self.sakPath + "/RailroadBig.exe"
            subprocess.call(startThis, shell=True)
            sleep(1)

        # start("controller", ip = "127.0.0.1", port = "1234", trace = "yes")
        elif name.lower()[:3] == "con":
            if ip == "":
                ip = "127.0.0.1"  # default to local host
            if port == "":
                port = "1234"  # default to the simulator
            if trace == "" or trace.lower()[:1] == "y":
                trace = "yes"  # default to trace on
            else:
                trace = "no"
            startThis = "start " + self.sakPath + "/StartController.exe" + \
                        " IP " + ip + \
                        " PORT " + port + \
                        " TRACE " + trace
            subprocess.call(startThis, shell=True)
            sleep(3)

        # start("ut4", ip = "127.0.0.1", port = "1234")
        elif name.lower()[:3] == "ut4":
            if ip == "":
                ip = "127.0.0.1"  # default to local host
            if port == "":
                port = "1234"  # default to the simulator
            startThis = "start " + self.sakPath + "/Throttle.exe" + \
                        " IP " + ip + \
                        " PORT " + port
            subprocess.call(startThis, shell=True)

        # start("RBLDisplay", ip = "127.0.0.1", port = "1235")
        elif name.lower()[:3] == "rbl":
            if ip == "":
                ip = "127.0.0.1"  # default to local host
            if port == "":
                port = "1235"  # default to the controller
            startThis = "start " + self.sakPath + "/RBLDisplay.exe" + \
                        " IP " + ip + \
                        " PORT " + port
            subprocess.call(startThis, shell=True)

        # start("adminthrottle",  ip = "127.0.0.1", port = "1235", layoutFile = "layout.xml", logs = "no")
        elif name.lower()[:3] == "adm":
            if ip == "":
                ip = "127.0.0.1"  # default to local host
            if port == "" or port == "1235":
                port = "1235"  # default to controller
                mode = "controller"  # default to controller mode
            else:
                mode = "standalone"  # default to standalone mode
            if layoutFile == "":
                layoutFile = "layout.xml"  # default to layout.xml
            if logs == "" or logs.lower()[:1] == "n":
                logs = "no"  # default to logs off
            else:
                logs = "yes"

            startThis = "start " + self.sakPath + "/adminthrottle.exe" + \
                        " IP " + ip + \
                        " PORT " + port + \
                        " MODE " + mode + \
                        " KEYBOARDLOG " + logs + \
                        " ADMINLOG " + logs
            if mode == "controller":
                startThis = startThis + " LAYOUTFILE " + layoutFile

            subprocess.call(startThis, shell=True)

        # Log it
        gLog.print("sak: " + startThis)

    def kill(self, name="all"):
        """
        I don't want to make this function static. It is easier to use as a member function."

        Usage (only uses first three letters of name)
        If name unrecognized, then defaults to "all"
          kill("simulator")
          kill("controller")
          kill("ut4")
          kill("RBLDisplay")
          kill("adminthrottle")
          kill("all")
        """
        if name.lower()[:3] == "sim":
            name = "RailroadBig"
        elif name.lower()[:3] == "con":
            name = "StartController"
        elif name.lower()[:3] == "ut4":
            name = "Throttle"
        elif name.lower()[:3] == "rbl":
            name = "RBLDisplay"
        elif name.lower()[:3] == "adm":
            name = "adminthrottle"
        elif name.lower()[:3] == "all":
            name = "all"
        else:
            name = "all"
        if name != "all":
            if "XP" in platform.platform():
                killThis = "tskill " + name
                subprocess.call(killThis, shell=True)
            else:
                killThis = "taskkill /T /IM " + name
                subprocess.call(killThis, shell=True)

            gLog.print("sak: " + killThis)
        else:
            if "XP" in platform.platform():
                subprocess.call("tskill RailroadBig", shell=True)
                subprocess.call("tskill StartController", shell=True)
                subprocess.call("tskill Throttle", shell=True)
                subprocess.call("tskill RBLDisplay", shell=True)
                subprocess.call("tskill adminthrottle", shell=True)
                gLog.print("sak: tskill RailroadBig, StartController, Throttle, RBLDisplay, AdminThrottle")
            else:
                subprocess.call("taskkill /T /IM RailroadBig", shell=True)
                subprocess.call("taskkill /T /IM StartController", shell=True)
                subprocess.call("taskkill /T /IM Throttle", shell=True)
                subprocess.call("taskkill /T /IM RBLDisplay", shell=True)
                subprocess.call("taskkill /T /IM adminthrottle", shell=True)
                gLog.print("sak: taskkill /T /IM RailroadBig, StartController, Throttle, RBLDisplay, AdminThrottle ")
