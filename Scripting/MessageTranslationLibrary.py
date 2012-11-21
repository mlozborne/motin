from collections import namedtuple

# Bit settings for messages
bitBackward          = '\x20' #  0010 0000
bitLightsOn          = '\x10' #  0001 0000 
bitHornOn            = '\x02' #  0000 0010
bitBellOn            = '\x01' #  0000 0001
bitMuteOn            = '\x08' #  0000 1000 F8 on UT4
bitCloseNextSwitch   = '\x01' #  0000 0001 F5 on UT4
bitThrowNextSwitch   = '\x02' #  0000 0010 F6 on UT4
bitRequestClose      = '\x30'
bitRequestThrow      = '\x10'

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
LocoDirfMsg    = namedtuple('LocoDirfMsg', 'slot', 'direction', 'lights', 'horn', 'bell')
LocoSndMsg     = namedtuple('LocoSndMsg' , 'slot', 'mute', 'closeNextSwitch', 'throwNextSwitch')
LocoSpdMsg     = namedtuple('LocoSpdMsg' , 'slot', 'speed')
SwReqMsg       = namedtuple('SwReqMsg'   , 'switch', 'direction') 

def makeCheckSumByte(str):
	checkSum = \xFF
	for byte in str:
		checkSum ^= byte
	return chr(checkSum)

def makeLocoSpdStr(msg):
	str = OPC_LOCO_SPD
	str += chr(msg.slot)
	str += chr(msg.speed)
	str += makeCheckSumByte(str)
	return str = "\x04\x00" + str

def makeLocoDirfStr(msg):
	str = OPC_LOCO_DIRF
	str += chr(msg.slot)
	dirf = x00
	if msg.direction == kBackward: dirf |= bitBackward
	if msg.lights == kOn         : dirf |= bitLightsOn
	if msg.horn == kOn           : dirf |= bitHornOn
	if msg.bell == kOn           : dirf |= bitBellOn
	str += chr(dirf)
	str += makeCheckSumByte(str)
	return str = "\x04\x00" + str
	
def makeLocoSndStr(msg):
	str = OPC_LOCO_SND
	snd = x00
	if msg.mute == kOn     : snd |= bitMuteOn
	if msg.closeNextSwitch : snd |= bitCloseNextSwitch
	if msg.throwNextSwitch : snd |= bitThrowNextSwitch
	str += chr(snd)
	str += makeCheckSumByte(str)
	return str = "\x04\x00" + str
	
def makeSwReqStr(msg):	
	str = OPC_SW_REQ
	str += chr(msg.switch - 1)
	if msg.direction == kClose:
		str += bitRequestClose
	else:
		str += bitRequestThrow
	str += makeCheckSumByte(str)
	return str = "\x04\x00" + str
	


