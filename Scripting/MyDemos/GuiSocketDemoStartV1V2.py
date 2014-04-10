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
        startThis = "start c:/python33/python.exe GuiSocketDemoV1.py 1 5000 5001"
        subprocess.call(startThis, shell=True)
        startThis = "start c:/python33/python.exe GuiSocketDemoV1.py 2 5001 5000"
        subprocess.call(startThis, shell=True)
    else:
        startThis = "start c:/python33/python.exe GuiSocketDemoV2.py Martin1 5000 5001"
        subprocess.call(startThis, shell=True)
        startThis = "start c:/python33/python.exe GuiSocketDemoV2.py Martin2 5001 5000"
        subprocess.call(startThis, shell=True)

    input("press enter to quit")

    if "XP" in platform.platform():
        killThis = "tskill python"
        subprocess.call(killThis, shell=True)
    else:
        killThis = "taskkill /T /IM python"
        subprocess.call(killThis, shell=True)
