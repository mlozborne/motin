from MessageTranslationLibrary import *
from TCP import *

def readLayoutFile(sk, fileName):
	sk.send(DoReadLayoutMsg(fileName = fileName))
	msg = sk.receive()
	while not isinstance(msg, PutReadLayoutResponseMsg):
		msg = sk.receive()
	time.sleep(3)
	return msg.responseFlag, msg.code
