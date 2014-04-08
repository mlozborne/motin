__author__ = 'Martin'

"""
This program starts the messageapp.
Press enter to quit and close messageapp windows.
"""

import subprocess
import platform

if __name__ == "__main__":
    choice = input("Choose (K)en's or (M)artin's version: ")
    if choice.upper() == "K":
        startThis = "start c:/python33/python.exe messageapp.py Ken1 4000 5000"
        subprocess.call(startThis, shell=True)
        startThis = "start c:/python33/python.exe messageapp.py Ken2 5000 4000"
        subprocess.call(startThis, shell=True)
    else:
        startThis = "start c:/python33/python.exe MyMessageApp.py Martin1 4000 5000"
        subprocess.call(startThis, shell=True)
        startThis = "start c:/python33/python.exe MyMessageApp.py Martin2 5000 4000"
        subprocess.call(startThis, shell=True)

    input("press enter to quit")

    if "XP" in platform.platform():
        killThis = "tskill python"
        subprocess.call(killThis, shell=True)
    else:
        killThis = "taskkill /T /IM python"
        subprocess.call(killThis, shell=True)
