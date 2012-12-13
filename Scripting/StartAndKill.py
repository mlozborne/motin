import subprocess
import platform

def startSimulator():
    subprocess.call("start ../runSoftware/RailroadBig.exe", shell=True)

def startController(ip = "127.0.0.1", port = "1234", trace = "yes"):
    subprocess.call("start ../runSoftware/StartController.exe IP " + ip + " PORT " + port + " TRACE " + trace, shell=True)

def killSimulator():
    if "XP" in platform.platform():
        subprocess.call("tskill RailroadBig.exe", shell=True)
    else:
        subprocess.call("taskkill /T /IM railroadbig.exe", shell=True)

def killController():
    if "XP" in platform.platform():
        subprocess.call("tskill StartController.exe", shell=True)
    else:
        subprocess.call("taskkill /T /IM StartController.exe", shell=True)

taskkill /T /IM startlocobufferserver.exe
taskkill /T /IM adminthrottle.exe
taskkill /T /IM throttle.exe




if __name__ == "__main__":
    print "platform = " + platform.platform()
    raw_input("...")
    
