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
OPC_SW_STATE   = '\xBC' # request state of a turnout
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

# Received message formats
InputRepMsg                 = namedtuple('InputRepMsg', 'sensor, isHi')
SwRepMsg                    = namedtuple('SwRepMsg', 'switch, direction')
LongAckMsg                  = namedtuple('LongAckMsg', 'responseToOpcode, switchState')
SlRdDataMsg                 = namedtuple('SlRdDataMsg', 'address, isAddressAlreadyInUse, slot')
PutTrainStateMsg            = namedtuple('PutTrainStateMsg', 'slot, state')
PutTrainPositionMsg         = namedtuple('PutTrainPositionMsg', 'slot, sensors')
PutSectionStateMsg          = namedtuple('PutSectionStateMsg', 'id, state')
PutSwitchStateMsg           = namedtuple('PutSwitchStateMsg', 'id, state')
PutSensorStateMsg           = namedtuple('PutSensorStateMsg', 'id, state')
PutInitOutcomeMsg           = namedtuple('PutInitOutcomeMsg', 'physAdd, physSlot, virtAdd, virtSlot')
PutReadLayoutResponseMsg    = namedtuple('PutReadLayoutResponseMsg', 'responseFlag, code')
PutTrainInformationMsg      = namedtuple('PutTrainInformationMsg', 'slot, speed, direction, lights, bell, horn, mute')
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
	else                                             : return None
	
def splitMsgStr(st):
	if st[0] != chr(0):
		opcode = st[0]
	else:
		opcode = st[1]
	if   opcode == OPC_INPUT_REP              : return splitInputRepStr(st)
	elif opcode == OPC_SW_REP                 : return splitSwRepStr(st)
	elif opcode == OPC_LONG_ACK               : return splitLongAckStr(st)
	elif opcode == OPC_SL_RD_DATA             : return splitSlRdDataStr(st)
	elif opcode == putTrainState              : return splitPutTrainStateStr(st)
	elif opcode == putTrainPosition           : return splitPutTrainPositionStr(st)
	elif opcode == putSectionState            : return splitPutSectionStateStr(st)
	elif opcode == putSwitchState             : return splitPutSwitchStateStr(st)
	elif opcode == putSensorState             : return splitPutSensorStateStr(st)
	elif opcode == putInitOutcome             : return splitPutInitOutcomeStr(st)
	elif opcode == putReadLayoutResponse      : return splitPutReadLayoutResponseStr(st)
	elif opcode == putTrainInformation        : return splitPutTrainInformationStr(st)
	elif opcode == putPowerChangeComplete     : return splitPutPowerChangeCompleteStr(st)
	else                                      : return st.encode("hex")

def makeCheckSumByte(st):
	checkSum = 0xFF
	for byte in st:
		checkSum ^= ord(byte)
	return chr(checkSum)

def makePowerStr(msg):
	#<0x83><CHK>    on
	#<0x82><CHK>    off
	if msg.setting == kOn:
		st = OPC_GPON
	else:
		st = OPC_GPOFF
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeLocoSpdStr(msg):
	#<0xA0><SLOT#><SPD><CHK> 
	st = OPC_LOCO_SPD
	st += chr(msg.slot)
	st += chr(min(127, msg.speed))
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st

def makeLocoDirfStr(msg):
	#<0xA1><SLOT#><DIR_STATE><CHK> 
	st = OPC_LOCO_DIRF
	st += chr(msg.slot)
	dirf = 0x00
	if msg.direction == kBackward: dirf |= bitBackward
	if msg.lights == kOn         : dirf |= bitLightsOn
	if msg.horn == kOn           : dirf |= bitHornOn
	if msg.bell == kOn           : dirf |= bitBellOn
	st += chr(dirf)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeLocoSndStr(msg):
	#<0xA2><SLOT#><SOUND><CHK>
	st = OPC_LOCO_SND
	st += chr(msg.slot)
	snd = 0x00
	if msg.mute == kOn   : snd |= bitMuteOn
	if msg.F5 == kOn     : snd |= bitF5
	if msg.F6 == kOn     : snd |= bitF6
	st += chr(snd)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeSwReqStr(msg):	
	#<0xB0><SW1><SW2><CHK> 
	st = OPC_SW_REQ
	st += chr(msg.switch - 1)
	if msg.direction == kClosed:
		st += chr(bitRequestClose)
	else:
		st += chr(bitRequestThrow)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeMoveSlotsStr(msg):
	#<0xBA><slot#><slot#><chk> 
	st = OPC_MOVE_SLOTS
	st += chr(msg.slot1)
	st += chr(msg.slot2)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeLocoAdrStr(msg):
	#<0xBF><adrhigh><adrlow><chk> 
	st = OPC_LOCO_ADR
	st += chr(msg.address / 128)
	st += chr(msg.address % 128)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeWriteSlotDataToClearStr(msg):
	#<0xEF><0E><slot#><status><adrlow><spd><dirf><trk><ss2><adrhigh><snd><id1><id2><chk>
	st = ["\x00" for i in range(0, 13)]
	st[0] = OPC_WR_SL_DATA
	st[1] = "\x0E"            # message length = 14
	st[2] = chr(msg.slot)  
	st[3] = "\x0B"            # status = 0000 1011
	st = ''.join(st)
	st += makeCheckSumByte(st)
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def convertNaturalToBytes(value):
	lowByte = value % 128
	highByte = value /128
	return chr(lowByte), chr(highByte)
	

def makeDoLocoInitStr(msg):
	#<0><8><physical loco address> <count> <sensor#>...<sensor#>      where address and sensor# are 2 bytes
	st = chr(0)
	st += doLocoInit
	lowByte, highByte = convertNaturalToBytes(msg.address)
	st += lowByte
	st += highByte
	sensorCount = len(msg.sensors)
	st += chr(sensorCount)
	for sensor in msg.sensors:
		lowByte, highByte = convertNaturalToBytes(sensor)
		st += lowByte
		st += highByte
	return chr(len(st)%128) + chr(len(st)/128) + st
	
def makeDoReadLayoutStr(msg):
	#<0><10><count><XML file name>       where count is 1 byte
	st = chr(0)
	st += doReadLayout
	st += chr(len(msg.fileName))
	st += msg.fileName
	return chr(len(st)%128) + chr(len(st)/128) + st	
	
def splitInputRepStr(st):
	#<0xB2><SN1><SN2><CHK>
	bitI      = 0x20
	bitL      = 0x10
	a = ord(st[1])
	b = ord(st[2]) & 0x0F
	c = 2 * (128 * b + a + 1)
	if (ord(st[2]) & bitI) == bitI:
		# bitI is 1
		sensor = c
	else:
		# bitI is 0
		sensor = c - 1
	isHi = ((ord(st[2]) & bitL) == bitL)
	return InputRepMsg(sensor = sensor, isHi = isHi)

def convertBytesToNatural(b1, b2):
	return ord(b1) + 128 * ord(b2)

def splitSwRepStr(st):
	#<0xB1><SW1><SW2><CHK> 
	b1 = st[1]
	b2 = chr(ord(st[2]) & 0x0F)
	switch = 1 + convertBytesToNatural(b1, b2)
	if (ord(st[2]) & bitReportClosed) == bitReportClosed: 
		direction = kClosed
	else:
		direction = kThrown
	return SwRepMsg(switch = switch, direction = direction)

def splitLongAckStr(st):
	#<0xB4><lopc><ack><CHK>
	responseToOpcode = chr(ord(st[1]) | 0x80)
	if responseToOpcode == OPC_SW_STATE:
		if st[2] == '\x30':
			state = kClosed
		else:	
			state = kThrown
	else:
		state = 0
	return LongAckMsg(responseToOpcode = responseToOpcode, switchState = state)

def splitSlRdDataStr(st):
	#<0xE7><0E><slot#><status><adrlow><spd><dirf><trk><ss2><adrhigh><snd><id1><id2><chk>
	return SlRdDataMsg(
	       address = convertBytesToNatural(st[4], st[9]), 
			 isAddressAlreadyInUse = ((ord(st[3]) & 0x30) == 0x30), 
			 slot = ord(st[2]))

def splitPutTrainStateStr(st):
	#<0><1><slot#> <train state>
	return PutTrainStateMsg(slot = ord(st[2]), state = ord(st[3]))
	
def splitPutTrainPositionStr(st):
	#<0><2><slot#><count><sensor#>...<sensor#>           where sensor# is 2 bytes
	slot = ord(st[2])
	sensorCount = ord(st[3])
	sensors = []
	for i in range (1, sensorCount+1):
		sensors.append(convertBytesToNatural(st[3 + 2*i-1], st[3 + 2*i])) 
	return PutTrainPositionMsg(slot = slot, sensors = sensors)

def splitPutSectionStateStr(st):
	#<0><3><section#> <section state>                  where section# is 2 bytes
	return PutSectionStateMsg(
	       id = convertBytesToNatural(st[2], st[3]), 
			 state = ord(st[4]))

def splitPutSwitchStateStr(st):
	#<0><4><switch#> <switch state>
	return PutSwitchStateMsg(
	       id = ord(st[2]), 
			 state = ord(st[3]))

def splitPutSensorStateStr(st):
	#<0><5><sensor#> <sensor state>                   where sensor# is 2 bytes
	return PutSensorStateMsg(
	       id = convertBytesToNatural(st[2], st[3]), 
			 state = ord(st[4]))

def splitPutInitOutcomeStr(st):
	#<0><9><physical loco address> <slot#> <virtual loco address> <slot#>   where addresses are 2 bytes
	return PutInitOutcomeMsg(
	       physAdd = convertBytesToNatural(st[2], st[3]), 
			 physSlot = ord(st[4]), 
			 virtAdd = convertBytesToNatural(st[5], st[6]), 
			 virtSlot = ord(st[7]))

def splitPutReadLayoutResponseStr(st):
	#<0><11><XML read response flag><code>          where code is 2 bytes
	return PutReadLayoutResponseMsg(
	       responseFlag = ord(st[2]), 
			 code = convertBytesToNatural(st[3], st[4]))

def splitPutTrainInformationStr(st):
	#<0><21><virtual slot#> <speed> <direction> <light> <bell> <horn> <mute>
   return PutTrainInformationMsg(
	       slot=ord(st[2]), speed=ord(st[3]), direction=ord(st[4]),
			 lights=ord(st[5]), bell=ord(st[6]), horn = ord(st[7]),
			 mute = ord(st[8]))
			 
def splitPutPowerChangeCompleteStr(st):
	#<0><26>
	return PutPowerChangeCompleteMsg(dummy = 0)

#########################################################################################
	
import unittest

def ex(n, st):                        # expand the sting with length and checkSum
	st += makeCheckSumByte(st)
	return chr(n%128) + chr(n/128) + st
	
def ex2(n, st):                       # expand the sting WITHOUT checksum
	return chr(n%128) + chr(n/128) + st

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
		
		msg = MoveSlotsMsg(slot1=1, slot2=2)
		self.assertEquals(ex(4, "\xBA\x01\x02"), makeMsgStr(msg))
		
		msg = LocoAdrMsg(address=1111)
		st = "\xBF" + chr(1111/128) + chr(1111%128)
		self.assertEquals(ex(4, st), makeMsgStr(msg))
		
		msg = WriteSlotDataToClearMsg(slot=1)
		st = "\xEF\x0E\x01\x0B\x00\x00\x00\x00\x00\x00\x00\x00\x00"
		self.assertEquals(ex(14, st), makeMsgStr(msg))
		
		msg = DoLocoInitMsg(address=1111, sensors=[1, 2, 3])
		st =  chr(0) + chr(8) + chr(1111%128) + chr(1111/128) + \
		      chr(3) + \
				chr(1) + chr(0) + \
				chr(2) + chr(0) + \
				chr(3) + chr(0)
		self.assertEquals(ex2(11, st), makeMsgStr(msg))
		
		msg = DoReadLayoutMsg(fileName="abc")
		st = chr(0) + chr(10) + chr(3) + "abc"
		self.assertEquals(ex2(6, st), makeMsgStr(msg))
		
	def testSplitMsgStr(self):
		st = "\xB2" + chr(0) + chr(0)
		st += makeCheckSumByte(st)
		self.assertEquals(InputRepMsg(sensor=1,isHi=False), splitInputRepStr(st))		
		st = "\xB2" + chr(4) + chr(0)
		st += makeCheckSumByte(st)
		self.assertEquals(InputRepMsg(sensor=9,isHi=False), splitInputRepStr(st))		
		st = "\xB2" + chr(4) + "\x10"
		st += makeCheckSumByte(st)
		self.assertEquals(InputRepMsg(sensor=9,isHi=True), splitInputRepStr(st))		
		st = "\xB2" + chr(121) + "\x31"
		st += makeCheckSumByte(st)
		self.assertEquals(InputRepMsg(sensor=500,isHi=True), splitInputRepStr(st))
		st = "\xB2" + chr(121) + "\x01"
		st += makeCheckSumByte(st)
		self.assertEquals(InputRepMsg(sensor=499,isHi=False), splitInputRepStr(st))
		
		st = "\xB1" + chr(115) + "\x33"
		st += makeCheckSumByte(st)
		self.assertEquals(SwRepMsg(switch=500, direction=kClosed), splitSwRepStr(st))

		st = "\xB4" + "\x3F" + "\x00"
		st += makeCheckSumByte(st)
		self.assertEquals(LongAckMsg(responseToOpcode=OPC_LOCO_ADR, switchState=0), \
		                  splitLongAckStr(st))

		st =  "\xE7\x0E\x01\x30" + \
		      chr(1111%128) + "\x00\x00\x00\x00" + chr(1111/128) + \
				"\x00\x00\x00"
		st += makeCheckSumByte(st)
		self.assertEquals(SlRdDataMsg(address=1111, isAddressAlreadyInUse=True, slot=1), \
		                  splitSlRdDataStr(st))

		st = chr(0) + chr(1) + chr(1) + chr(kMoving)
		self.assertEquals(PutTrainStateMsg(slot=1, state=kMoving), splitPutTrainStateStr(st))
		
		st =  chr(0) + chr(2) + chr(1) + chr(3) + \
		      chr(73) + chr(1) + \
				chr(74) + chr(1) + \
				chr(75) + chr(1)
		self.assertEquals(PutTrainPositionMsg(slot=1, sensors=[201,202,203]), \
		                  splitPutTrainPositionStr(st))
		
		st = chr(0) + chr(3) + chr(72) + chr(1) + chr(kFree)
		self.assertEquals(PutSectionStateMsg(id=200, state=kFree), splitPutSectionStateStr(st))
		
		st = chr(0) + chr(4) + chr(99) + chr(kBeginClosed)
		self.assertEquals(PutSwitchStateMsg(id=99, state=kBeginClosed), splitPutSwitchStateStr(st))
		
		st = chr(0) + chr(5) + chr(116) + chr(3) + chr(kSensorOpen)
		self.assertEquals(PutSensorStateMsg(id=500, state=kSensorOpen), splitPutSensorStateStr(st))
		
		st =  chr(0) + chr(9) + \
				chr(1111%128) + chr(1111/128) + chr(1) + \
				chr(11) + chr(0) + chr(2)
		self.assertEquals(PutInitOutcomeMsg(physAdd=1111, physSlot=1, virtAdd=11, virtSlot=2), \
		                  splitPutInitOutcomeStr(st))
		
		st =  chr(0) + chr(11) + chr(100) + chr(72) + chr(1)
		self.assertEquals(PutReadLayoutResponseMsg(responseFlag=100, code=200), \
								splitPutReadLayoutResponseStr(st))
								
		st =  chr(0) + chr(21) + chr(1) + chr(100) + chr(kForward) + \
		      chr(kOn) + chr(kOff) + chr(kOn) + chr(kOn)
		self.assertEquals(PutTrainInformationMsg(slot=1, speed=100, direction=kForward,
		                                         lights=kOn, bell=kOff, horn=kOn, mute=kOn), \
															  splitPutTrainInformationStr(st))
															  
		st = chr(0) + chr(26)
		self.assertEquals(PutPowerChangeCompleteMsg(dummy=0), splitPutPowerChangeCompleteStr(st))

def runTest():
	suite = unittest.makeSuite(TestMessageTranslationLibrary)
	unittest.TextTestRunner().run(suite)
