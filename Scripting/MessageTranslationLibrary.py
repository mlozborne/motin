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
bitReportClosed      = 0x30
bitReportThrown      = 0x20


# Some message constants
kOn                   = 1        # For lights, horn, bell, mute, power
kOff                  = 0

kForward              = 0        # For train direction
kBackward             = 1

kMoving               = 0        # For train states
kWaiting              = 1
kHalted               = 2
kError                = 3
kBeginChangeDirection = 4
kBeginWaiting         = 5
kBeginHalted          = 6

kFree                 = 0        # For section states
kReserved             = 1
kOccupied             = 2
kBlocked              = 3

kClosed               = 0        # For switch states
kThrown               = 1
kBeginClosed          = 2
kBeginThrown          = 3
kRead                 = 4
kUnknown              = 5

kSensorOpen           = 0        # For sensor states 
kSensorClosed         = 1


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
OPC_INPUT_REP          = '\xB2' # report sensor fired
OPC_SW_REP             = '\xB1' # report turnout now closed/thrown
OPC_LONG_ACK           = '\xB4' # if 2nd byte = 3F, then insufficient slots
OPC_SL_RD_DATA         = '\xE7' # slot data response
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
InputRepMsg                 = namedtuple('InputRepMsg', 'sensor, isHi')
SwRepMsg                    = namedtuple('SwRepMsg', 'switch, direction')
LongAckMsg                  = namedtuple('LongAckMsg', 'responseToOpcode, switchState')
SlRdDataMsg                 = namedtuple('SlRdDataMsg', 'address, isAddressAlreadyInUse, slot')
PutTrainStateMsg            = namedtuple('PutTrainStateMsg', 'slot, state')
PutTrainPositionMsg         = namedtuple('PutTrainPositionMsg', 'slot, sensors')
PutSectionStateMsg          = namedtuple('PutSectionStateMsg', 'id, state')
PutSwitchStateMsg           = namedtuple('PutSwitchStateMsg', 'id, switchState')
PutSensorStateMsg           = namedtuple('PutSensorStateMsg', 'id, state')
PutInitOutcomeMsg           = namedtuple('PutInitOutcomeMsg', 'physAdd, physSlot, virtAdd, virtSlot')
PutReadLayoutResponseMsg    = namedtuple('PutReadLayoutResponseMsg', 'responseFlag, code')
PutTrainInformationMsg      = namedtuple('PutTrainInformationMsg', 'slot, speed, direction, lights, bell, horn')
PutPowerChangeCompleteMsg   = namedtuple('PutPowerChangeCompleteMsg', 'dummy')

def makeMsgStr(msg):
	if   isinstance(msg, PowerMsg)		   	   	 : return makePowerStr(msg)
	elif isinstance(msg, LocoSpdMsg)                 : return makeLocoSpdStr(msg)
	elif isinstance(msg, LocoDirfMsg)                : return makeLocoDirfStr(msg)
	elif isinstance(msg, LocoSndMsg)                 : return makeLocoSndStr(msg)
	elif isinstance(msg, SwReqMsg)                   : return makeSwReqStr(msg)
	elif isinstance(msg, MoveSlotsMsg)               : return makeMoveSlotsStr(msg)
	elif isinstance(msg, LocoAdrMsg)                 : return makeLocoAdrStr(msg)
	elif isinstance(msg, WriteSlotDataToClearMsg)    : return makeWriteSlotDataToClearStr(msg)
	elif isinstance(msg, DoLocoInitMsg)              : return makeDoLocoInitStr(msg)
	elif isinstance(msg, DoReadLayoutMsg)            : return makeDoReadLayoutStr(msg)
	elif isinstance(msg, GetSwitchStatesMsg)         : return makeGetSwitchStatesStr(msg)
	else                                             : return None
	
def splitMsgStr(str):
	if str[0] != 0:
		opcode = str[0]
	else:
		opcode = str[1]
	if   opcode == OPC_INPUT_REP              : return splitInputRepStr(str)
	elif opcode == OPC_SW_REP                 : return splitSwRepStr(str)
	elif opcode == OPC_LONG_ACK               : return splitLongAckStr(str)
	elif opcode == OPC_SL_RD_DATA             : return splitSlRdDataStr(str)
	elif opcode == putTrainState              : return splitPutTrainStateStr(str)
	elif opcode == putTrainPosition           : return splitPutTrainPositionStr(str)
	elif opcode == putSectionState            : return splitPutSectionStateStr(str)
	elif opcode == putSwitchState             : return splitPutSwitchStateStr(str)
	elif opcode == putSensorState             : return splitPutSensorStateStr(str)
	elif opcode == putInitOutcome             : return splitPutInitOutcomeStr(str)
	elif opcode == putReadLayoutResponse      : return splitPutReadLayoutResponseStr(str)
	elif opcode == putTrainInformation        : return splitPutTrainInformationStr(str)
	elif opcode == putPowerChangeComplete     : return splitPutPowerChangeCompleteStr(str)
	else                                      : return none 

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
	return chr(len(str)) + chr(0) + str
	
def makeLocoSpdStr(msg):
	str = OPC_LOCO_SPD
	str += chr(msg.slot)
	str += chr(min(127, msg.speed))
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str

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
	return chr(len(str)) + chr(0) + str
	
def makeLocoSndStr(msg):
	str = OPC_LOCO_SND
	str += chr(msg.slot)
	snd = 0x00
	if msg.mute == kOn   : snd |= bitMuteOn
	if msg.F5 == kOn     : snd |= bitF5
	if msg.F6 == kOn     : snd |= bitF6
	str += chr(snd)
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
def makeSwReqStr(msg):	
	str = OPC_SW_REQ
	str += chr(msg.switch - 1)
	if msg.direction == kClosed:
		str += chr(bitRequestClose)
	else:
		str += chr(bitRequestThrow)
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
def makeMoveSlotsStr(msg):
	str = OPC_MOVE_SLOTS
	str += chr(msg.slot1)
	str += chr(msg.slot2)
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
def makeLocoAdrStr(msg):
	str = OPC_LOCO_ADR
	str += chr(msg.address / 128)
	str += chr(msg.address % 128)
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
def makeWriteSlotDataToClearStr(msg):
	str = ["\x00" for i in range(0, 14)]
	str[0] = OPC_WR_SL_DATA
	str[1] = "\x0E"            # message length = 14
	str[2] = chr(msg.slot)  
	str[3] = "\x0B"            # status = 0000 1011
	str = ''.join(str)
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
def convertNaturalToBytes(value):
	lowByte = value % 128
	highByte = value /128
	return chr(lowByte), chr(highByte)

def makeDoLocoInitStr(msg):
	str = chr(0)
	str += doLocoInit
	lowByte, highByte = convertNaturalToBytes(msg.address)
	str += lowByte
	str += highByte
	sensorCount = len(sensors)
	str += chr(sensorCount)
	for sensor in msg.sensors:
		lowByte, highByte = convertNaturalToBytes(sensor)
		str += lowByte
		str += highByte
	str += makeCheckSumByte(str)
	return chr(len(str)) + chr(0) + str
	
"""def makeDoReadLayoutStr(msg)
	msg.fileName """
	

def splitInputRepStr(str):
	bitI      = 0x20
	bitL      = 0x10
	a = ord(str[1])
	b = ord(str[2]) & 0x0F
	c = 2 * (128 * b + a + 1)
	if (str[2] & bitI) == bitI:
		# bitI is 1
		sensor = c
	else:
		# bitI is 0
		sensor = c - 1
	isHi = ((str[2] & bitL) == bitL)
	return InputRepMsg(sensor = sensor, isHi = isHi)

def splitSwRepStr(str):
	switch = 1 + ord(str[1]) + 128 * (ord(str[2]) & 0x0F);
	if (ord(str[2]) & kReportClosed) == kReportClosed: 
		direction = closed
	else:
		direction = thrown
	return SwRepMsg(switch = switch, direction = direction)

def splitLongAckStr(str):
	responseToOpcode = ord(str[1]) | 0x80
	if responseToOpcode == OPC_SW_STATE:
		if str[2] == '\x30':
			state = kClosed
		else:	
			state = kThrown
	return LongAckMsg(responseToOpcode = responseToOpcode, switchState = state)

def convertBytesToNatural(b1, b2):
	return ord(b1) + 128 * ord(b2)

def splitSlRdDataStr(str):
	return SlRdDataMsg(
	       address = convertBytesToNatural(str[4], str[9]), 
			 isAddressAlreadyInUse = ((ord(str[3]) & 0x30) == 0x30), 
			 slot = ord(str[2]))

def splitPutTrainStateStr(str):
	return PutTrainStateMsg(slot = ord(str[2]), state = ord(str[3]))
	
def splitPutTrainPositionStr(str):
	slot = ord(str[2])
	sensorCount = ord(str[3])
	sensors = []
	for i in range (1, sensorCount+1):
		sensors.append(convertBytesToNatural(str[3 + 2*1-1], str[3 + 2*i])) 
	return PutTrainPositionMsg(slot = slot, sensors = sensors)

def splitPutSectionStateStr(str):
	return PutSensorStateMsg(
	       id = convertBytesToNatural(str[2], str[3]), 
			 state = ord(str[4]))

def splitPutSwitchStateStr(str):
	return PutSwitchStateMsg(
	       id = ord(str[2]), 
			 state = ord(str[3]))

def splitPutSensorStateStr(str):
	return PutSensorStateMsg(
	       id = convertBytesToNatural(str[2], str[3]), 
			 state = ord(str[4]))

def splitPutInitOutcomeStr(str):
	return PutInitOutcomeMsg(
	       physAdd = convertBytesToNatural(str[2], str[3]), 
			 physSlot = ord(str[4]), 
			 virtAdd = convertBytesToNatural(str[5], str[6]), 
			 virtSlot = ord(str[7]))

def splitPutReadLayoutResponseStr(str):
	return PutReadLayoutResponseMsg(
	       responseFlag = ord(str[2]), 
			 code = convertBytesToNatural(str[3], str[4]))

def splitPutTrainInformationStr(str):
   return PutTrainInformationMsg(
	       slot=ord(str[2]), speed=ord(str[3]), direction=ord(str[4]),
			 lights=ord(str[5]), bell=ord(str[6]), horn = ord(str[7]),
			 mute = ord(str[8]))
			 
def splitPutPowerChangeCompleteStr(str):
	return PutPowerChangeCompleteMsg(dummy = 0)

#########################################################################################
	
import unittest

def ex(n, str):                        # extend the string with length and checkSum
	str += makeCheckSumByte(str)
	return chr(n) + chr(0) + str

class TestMessageTranslationLibrary(unittest.TestCase):

	def testMakeCheckSumByte(self):
		self.assertEquals("\xFF", makeCheckSumByte("\x00"))
		self.assertEquals("\x00", makeCheckSumByte("\xF0\x0F"))
		self.assertEquals("\xFF", makeCheckSumByte("\xF0\x0F\xFF"))
		self.assertEquals("\x5C", makeCheckSumByte("\xA0\x01\x02"))
				
	def testMakeMsgStr(self):
		msg = PowerMsg(setting=kOn)
		self.assertEquals(ex(2, "\x83"), makeMsgStr(msg))
		msg = PowerMsg(setting=kOff)
		self.assertEquals(ex(2, "\x82"), makeMsgStr(msg))

		msg = LocoSpdMsg(slot=1, speed=2)
		self.assertEquals(ex(4, "\xA0\x01\x02"),  makeMsgStr(msg))
		msg = LocoSpdMsg(slot=1, speed=300)
		self.assertEquals(ex(4, "\xA0\x01\x7F"), makeMsgStr(msg))

		msg = LocoDirfMsg(slot=1, direction=kBackward, lights=kOn, horn=kOn, bell=kOn)
		self.assertEquals(ex(4, "\xA1\x01\x33"), makeMsgStr(msg))
		msg = LocoDirfMsg(slot=1, direction=kForward, lights=kOff, horn=kOff, bell=kOff)
		self.assertEquals(ex(4, "\xA1\x01\x00"), makeMsgStr(msg))

		msg = LocoSndMsg(slot=1, mute=kOn, F5=kOn, F6=kOn)
		self.assertEquals(ex(4, "\xA2\x01\x0B"), makeMsgStr(msg))
		msg = LocoSndMsg(slot=1, mute=kOff, F5=kOff, F6=kOff)
		self.assertEquals(ex(4, "\xA2\x01\x00"), makeMsgStr(msg))

		msg = SwReqMsg(switch=1, direction=kClosed)
		self.assertEquals(ex(4, "\xB0\x00\x30"), makeMsgStr(msg))
		msg = SwReqMsg(switch=1, direction=kThrown)
		self.assertEquals(ex(4, "\xB0\x00\x10"), makeMsgStr(msg))
		
#		msg = MoveSlotsMsg(slot1=1, slot2=2)
#		self.assertEquals(ex(4, "\xBA\x01\x02"), makeMsgStr(msg))

def runTest():
	suite = unittest.makeSuite(TestMessageTranslationLibrary)
	unittest.TextTestRunner().run(suite)
