from collections import namedtuple

# Bit settings for messages
bitBackward          = 0x20 #  0010 0000
bitLightsOn          = 0x10 #  0001 0000 
bitHornOn            = 0x02 #  0000 0010
bitBellOn            = 0x01 #  0000 0001
bitMuteOn            = 0x08 #  0000 1000 F8 on UT4
bitF5                = 0x01 #  0000 0001 F5 on UT4, change in value tells controller to close next switch 
bitF6                = 0x02 #  0000 0010 F6 on UT4, change in value tells controller to throw next switch
bitRequestClose      = 0x30
bitRequestThrow      = 0x10

# Some message constants
kOn         = True
kOff        = False
kForward    = True
kBackward   = False
kClose      = True
kThrow      = False

# Message opcodes
OPC_LOCO_SPD   = '\xA0' # set speed
OPC_LOCO_DIRF  = '\xA1' # set direction, horn, bell, lights
OPC_LOCO_SND   = '\xA2' # set mute and unmute sound
OPC_SW_REQ     = '\xB0' # move a turnout

# Message formats
LocoDirfMsg    = namedtuple('LocoDirfMsg', 'slot, direction, lights, horn, bell')
LocoSndMsg     = namedtuple('LocoSndMsg' , 'slot, mute, F5, F6')
LocoSpdMsg     = namedtuple('LocoSpdMsg' , 'slot, speed')
SwReqMsg       = namedtuple('SwReqMsg'   , 'switch, direction') 

def makeCheckSumByte(str):
	checkSum = 0xFF
	for byte in str:
		checkSum ^= ord(byte)
	return chr(checkSum)

def makeLocoSpdStr(msg):
	str = OPC_LOCO_SPD
	str += chr(msg.slot)
	str += chr(min(127, msg.speed))
	str += makeCheckSumByte(str)
	return "\x04\x00" + str

def makeLocoDirfStr(msg):
	str = OPC_LOCO_DIRF
	str += chr(msg.slot)
	dirf = 0x00
	if msg.direction == kBackward: dirf |= bitBackward
	if msg.lights == kOn         : dirf |= bitLightsOn
	if msg.horn == kOn           : dirf |= bitHornOn
	if msg.bell == kOn           : dirf |= bitBellOn
	str += chr(dirf)
	str += makeCheckSumByte(str)
	return  "\x04\x00" + str
	
def makeLocoSndStr(msg):
	str = OPC_LOCO_SND
	str += chr(msg.slot)
	snd = 0x00
	if msg.mute == kOn   : snd |= bitMuteOn
	if msg.F5 == kOn     : snd |= bitF5
	if msg.F6 == kOn     : snd |= bitF6
	str += chr(snd)
	str += makeCheckSumByte(str)
	return  "\x04\x00" + str
	
def makeSwReqStr(msg):	
	str = OPC_SW_REQ
	str += chr(msg.switch - 1)
	if msg.direction == kClose:
		str += chr(bitRequestClose)
	else:
		str += chr(bitRequestThrow)
	str += makeCheckSumByte(str)
	return  "\x04\x00" + str

#########################################################################################
	
import unittest

class TestMessageTranslationLibrary(unittest.TestCase):
	def testMakeCheckSumByte(self):
		self.assertEquals("\x00", makeCheckSumByte("\xF0\x0F"))
		self.assertEquals("\xFF", makeCheckSumByte("\xF0\x0F\xFF"))
		self.assertEquals("\x5C", makeCheckSumByte("\xA0\x01\x02"))
		
	def testMakeLocoSpdStr(self):
		self.assertEquals("\x04\x00\xA0\x01\x02\x5C", makeLocoSpdStr(LocoSpdMsg(slot=1, speed=2)))
		self.assertEquals("\x04\x00\xA0\x01\x7F\x21", makeLocoSpdStr(LocoSpdMsg(slot=1, speed=300)))
		
	def testMakeLocoDirfStr(self):
		self.assertEquals("\x04\00\xA1\x01\x33\x6C", makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOn, bell=kOn)))
		self.assertEquals("\x04\00\xA1\x01\x00\x5F", makeLocoDirfStr(LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff)))

	def testMakeLocoSndStr(self):
		self.assertEquals("\x04\00\xA2\x01\x0B\x57", makeLocoSndStr(LocoSndMsg(slot=1, mute=kOn, F5=kOn, F6=kOn)))
		self.assertEquals("\x04\00\xA2\x01\x00\x5C", makeLocoSndStr(LocoSndMsg(slot=1, mute=kOff, F5=kOff, F6=kOff)))

	def testMakeSwReqStr(self):
		self.assertEquals("\x04\00\xB0\x00\x30\x7F", makeSwReqStr(SwReqMsg(switch=1, direction=kClose)))
		self.assertEquals("\x04\00\xB0\x00\x10\x5F", makeSwReqStr(SwReqMsg(switch=1, direction=kThrow)))

def runTest():
	suite = unittest.makeSuite(TestMessageTranslationLibrary)
	unittest.TextTestRunner().run(suite)
