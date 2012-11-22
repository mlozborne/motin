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

# Opcodes sent to controller/railroad
OPC_GPOFF      = '\x82' # power off
OPC_GPON       = '\x83' # power on
OPC_LOCO_SPD   = '\xA0' # set speed
OPC_LOCO_DIRF  = '\xA1' # set direction, horn, bell, lights
OPC_LOCO_SND   = '\xA2' # set mute and unmute sound, close/throw next switch
OPC_SW_REQ     = '\xB0' # move a turnout
OPC_MOVE_SLOTS = '\xBA' # set a slot to in-use	
OPC_LOCO_ADR   = '\xBF' # request for slot data
OPC_WR_SL_DATA = '\xEF' # write data into a slot
doLocoInit             = chr(8)
doReadLayout           = chr(10)
getSwitchStates        = chr(22)
	
# Opcodes received from controller/railroad
OPC_INPUT_REP  = '\xB2' # report sensor fired
OPC_SW_REP     = '\xB1' # report turnout now open/thrown
OPC_LONG_ACK   = '\xB4' # if 2nd byte = 3F, then insufficient slots
OPC_SL_RD_DATA = '\xE7' # slot data response
putTrainState          = chr(1)
putTrainPosition       = chr(2)
putSectionState        = chr(3)
putSwitchState         = chr(4)
putSensorState         = chr(5)
putInitOutcome         = chr(9)
putReadLayoutResponse  = chr(11)
putTrainInformation    = chr(21)
putPowerChangeComplete = chr(26)

# Sent message formats
PowerMsg                   = namedtuple('PowerMsg', 'setting')
LocoSpdMsg                 = namedtuple('LocoSpdMsg' , 'slot, speed')
LocoDirfMsg                = namedtuple('LocoDirfMsg', 'slot, direction, lights, horn, bell')
LocoSndMsg                 = namedtuple('LocoSndMsg' , 'slot, mute, F5, F6')
SwReqMsg                   = namedtuple('SwReqMsg'   , 'switch, direction') 
MoveSlotsMsg               = namedtuple('MoveSlotsMsg', 'slot1, slot2')
LocoAdrMsg                 = namedtuple('LocoAdrMsg', 'address')
WriteSlotDataToClearMsg    = namedtuple('WriteSlotDataToClearMsg', 'slot')
DoLocoInitMsg              = namedtuple('DoLocoInitMsg', 'address, sensors')
DoReadLayoutMsg            = namedtuple('DoReadLayoutMsg', 'fileName')
GetSwitchStatesMsg         = namedtuple('GetSwitchStatesMsg', 'dummy')

# Received message formats
InputRepMsg
SwRepMsg
LongAckMsg
SlRdDataMsg
PutTrainStateMsg
PutTrainPositionMsg
PutSectionStateMsg
PutSwitchStateMsg
PutSensorStateMsg
PutInitOutcomeMsg
PutReadLayoutResponseMsg
PutTrainInformationMsg
PutPowerChangeCompleteMsg

def makeMsgStr(msg):
	if isinstance(msg, LocoDirfMsg)                  : return makeLocoDirfStr(msg)
	elif isinstance(msg, LocoSndMsg)                 : return makeLocoSndStr(msg)
	elif isinstance(msg, LocoSpdMsg)                 : return makeLocoSpdStr(msg)
	elif isinstance(msg, SwReqMsg)                   : return makeSwReqStr(msg)
	elif isinstance(msg, PowerMsg)                   : return makePowerStr(msg)
	elif isinstance(msg, MoveSlotsMsg)               : return makeMoveSlotsStr(msg)
	elif isinstance(msg, LocoAdrMsg)                 : return makeLocoAdrStr(msg)
	elif isinstance(msg, WriteSlotDataToClearMsg)    : return makeWriteSlotDataToClearStr(msg)
	elif isinstance(msg, DoLocoInitMsg)              : return makeDoLocoInitStr(msg)
	elif isinstance(msg, DoReadLayoutMsg)            : return makeDoReadLayoutStr(msg)
	elif isinstance(msg, GetSwitchStatesMsg)         : return makeGetSwitchStatesStr(msg)
	else: return None
	
def makeCheckSumByte(str):
	checkSum = 0xFF
	for byte in str:
		checkSum ^= ord(byte)
	return chr(checkSum)

def makePowerStr(msg):
	if msg.setting == kOn:
		str = OPC_GPON
	else:
		str = OPC_GPOFF
	str += makeCheckSumByte(str)
	return "\x02\x00" + str
	
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

	def testMakeMsgStr(self):
		msg = LocoSpdMsg(slot=1, speed=2)
		self.assertEquals("\x04\x00\xA0\x01\x02\x5C", makeMsgStr(msg))
		msg = LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOn, bell=kOn)
		self.assertEquals("\x04\00\xA1\x01\x33\x6C", makeMsgStr(msg))
		msg = LocoSndMsg(slot=1, mute=kOn, F5=kOn, F6=kOn)
		self.assertEquals("\x04\00\xA2\x01\x0B\x57", makeMsgStr(msg))
		msg = SwReqMsg(switch=1, direction=kClose)
		self.assertEquals("\x04\00\xB0\x00\x30\x7F", makeMsgStr(msg))		

def runTest():
	suite = unittest.makeSuite(TestMessageTranslationLibrary)
	unittest.TextTestRunner().run(suite)