"""
Representation of messages.
    o byte array during transmition via sockets
    o named tupes internally
Conversion operations
    o makeMsgStr(msg)    named tuple --> byte array
    o splitMsgStr(st)    int array   --> named tuple
"""
from MessageTranslationTypes import *

#######################################################################
# Top level make and split
def makeMsgStr(msg):
    if   isinstance(msg, PowerMsg): return makePowerStr(msg)
    elif isinstance(msg, LocoSpdMsg): return makeLocoSpdStr(msg)
    elif isinstance(msg, LocoDirfMsg): return makeLocoDirfStr(msg)
    elif isinstance(msg, LocoSndMsg): return makeLocoSndStr(msg)
    elif isinstance(msg, SwReqMsg): return makeSwReqStr(msg)
    elif isinstance(msg, MoveSlotsMsg): return makeMoveSlotsStr(msg)
    elif isinstance(msg, LocoAdrMsg): return makeLocoAdrStr(msg)
    elif isinstance(msg, WriteSlotDataToClearMsg): return makeWriteSlotDataToClearStr(msg)
    elif isinstance(msg, DoLocoInitMsg): return makeDoLocoInitStr(msg)
    elif isinstance(msg, DoReadLayoutMsg): return makeDoReadLayoutStr(msg)
    else: return msg
	
def splitMsgStr(st):
    if st[0] != 0:
        opcode = st[0]
    else:
        opcode = st[1]
    if   opcode == OPC_INPUT_REP: return splitInputRepStr(st)
    elif opcode == OPC_SW_REP: return splitSwRepStr(st)
    elif opcode == OPC_LONG_ACK: return splitLongAckStr(st)
    elif opcode == OPC_SL_RD_DATA: return splitSlRdDataStr(st)
    elif opcode == putTrainState: return splitPutTrainStateStr(st)
    elif opcode == putTrainPosition: return splitPutTrainPositionStr(st)
    elif opcode == putSectionState: return splitPutSectionStateStr(st)
    elif opcode == putSwitchState: return splitPutSwitchStateStr(st)
    elif opcode == putSensorState: return splitPutSensorStateStr(st)
    elif opcode == putInitOutcome: return splitPutInitOutcomeStr(st)
    elif opcode == putReadLayoutResponse: return splitPutReadLayoutResponseStr(st)
    elif opcode == putTrainInformation: return splitPutTrainInformationStr(st)
    elif opcode == putPowerChangeComplete: return splitPutPowerChangeCompleteStr(st)
    else: return st


#######################################################################
# Utility routines
def utMakeCheckSumByte(st):
    checkSum = 0xFF
    for byte in st:
        checkSum ^= byte
    return checkSum

def utConvertNaturalToBytes(value):
    lowByte = value % 128
    highByte = value // 128
    return lowByte, highByte

def utConvertBytesToNatural(b1, b2):
    return b1 + 128 * b2

def utConvertListToStr(lst):
    result = ""
    for x in lst:
        result += chr(x)
    return result

#######################################################################
# Make routines
def makePowerStr(msg):
    #<0x83><CHK>    on
    #<0x82><CHK>    off
    assert(msg.setting in kPowerValues)
    if msg.setting == kOn:
        st = [OPC_GPON]
    else:
        st = [OPC_GPOFF]
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return bytearray([lowByte, highByte] + st)
	
def makeLocoSpdStr(msg):
    #<0xA0><SLOT#><SPD><CHK>
    assert(kSlotMin <= msg.slot <= kSlotMax and msg.speed >= kSpeedMin)
    st = [OPC_LOCO_SPD]
    st.append(msg.slot)
    st.append(min(kSpeedMax, msg.speed))
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)

def makeLocoDirfStr(msg):
    #<0xA1><SLOT#><DIR_STATE><CHK>
    assert(
        kSlotMin <= msg.slot <= kSlotMax and \
        msg.direction in kDirectionValues and \
        msg.lights in kLightsValues and \
        msg.horn in kHornValues and \
        msg.bell in kBellValues)
    st = [OPC_LOCO_DIRF]
    st.append(msg.slot)
    dirf = 0x00
    if msg.direction == kBackward: dirf |= bitBackward
    if msg.lights == kOn: dirf |= bitLightsOn
    if msg.horn == kOn: dirf |= bitHornOn
    if msg.bell == kOn: dirf |= bitBellOn
    st.append(dirf)
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeLocoSndStr(msg):
    #<0xA2><SLOT#><SOUND><CHK>
    assert(
        kSlotMin <= msg.slot <= kSlotMax and \
        msg.mute in kMuteValues and \
        msg.F5 in kF5Values and \
        msg.F6 in kF6Values)
    st = [OPC_LOCO_SND]
    st.append(msg.slot)
    snd = 0x00
    if msg.mute == kOn: snd |= bitMuteOn
    if msg.F5 == kOn: snd |= bitF5
    if msg.F6 == kOn: snd |= bitF6
    st.append(snd)
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeSwReqStr(msg):	
    #<0xB0><SW1><SW2><CHK>
    assert(kSwitchMin <= msg.switch <= kSwitchMax and msg.direction in kDirectionValues)
    st = [OPC_SW_REQ]
    st.append(msg.switch - 1)
    if msg.direction == kClosed:
        st.append(bitRequestClose)
    else:
        st.append(bitRequestThrow)
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeMoveSlotsStr(msg):
    #<0xBA><slot#><slot#><chk>
    assert(kSlotMin <= msg.slot1 <= kSlotMax and kSlotMin <= msg.slot2 <= kSlotMax)
    st = [OPC_MOVE_SLOTS]
    st.append(msg.slot1)
    st.append(msg.slot2)
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeLocoAdrStr(msg):
    #<0xBF><adrhigh><adrlow><chk>
    assert(kLocoAddressMin <= msg.address <= kLocoAddressMax)
    st = [OPC_LOCO_ADR]
    st.append(msg.address // 128)
    st.append(msg.address % 128)
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeWriteSlotDataToClearStr(msg):
    #<0xEF><0E><slot#><status><adrlow><spd><dirf><trk><ss2><adrhigh><snd><id1><id2><chk>
    assert(kSlotMin <= msg.slot <= kSlotMax)
    st = [0 for x in range(0, 13)]
    st[0] = OPC_WR_SL_DATA
    st[1] = 0x0E            # message length = 14
    st[2] = msg.slot
    st[3] = 0x0B            # status = 0000 1011
    st.append(utMakeCheckSumByte(st))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
def makeDoLocoInitStr(msg):
    #<0><8><physical loco address> <count> <sensor#>...<sensor#>      where address and sensor# are 2 bytes
    assert(kLocoAddressMin <= msg.address <= kLocoAddressMax)
    for x in msg.sensors:
        assert(kSensorMin <= x <= kSensorMax)
    st = [0]
    st.append(doLocoInit)
    lowByte, highByte = utConvertNaturalToBytes(msg.address)
    st.append(lowByte)
    st.append(highByte)
    sensorCount = len(msg.sensors)
    st.append(sensorCount)
    for sensor in msg.sensors:
        lowByte, highByte = utConvertNaturalToBytes(sensor)
        st.append(lowByte)
        st.append(highByte)
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)
	
#def makeDoReadLayoutStr(msg):
#    #<0><10><count><XML file name>       where count is 1 byte
#    assert(isinstance(msg.fileName, str))
#    fileName = bytes(msg.fileName, 'ascii')
#    st = [0]
#    st.append(doReadLayout)
#    st.append(len(fileName))
#    st += list(fileName)
#    lowByte, highByte = utConvertNaturalToBytes(len(st))
#    return utConvertListToStr([lowByte, highByte] + st)

def makeDoReadLayoutStr(msg):
    #<0><10><count><XML file name>       where count is 1 byte
    assert(isinstance(msg.fileName, str))
    st = [0]
    st.append(doReadLayout)
    st.append(len(msg.fileName))
    for chr in msg.fileName:
        st.append(ord(chr))
    lowByte, highByte = utConvertNaturalToBytes(len(st))
    return utConvertListToStr([lowByte, highByte] + st)

#######################################################################
# Split routines
def splitInputRepStr(st):
    #<0xB2><SN1><SN2><CHK>
    bitI      = 0x20
    bitL      = 0x10
    a = st[1]
    b = st[2] & 0x0F
    c = 2 * (128 * b + a + 1)
    if (st[2] & bitI) == bitI:
        # bitI is 1
        sensor = c
    else:
        # bitI is 0
        sensor = c - 1
    isHi = ((st[2] & bitL) == bitL)
    return InputRepMsg(sensor=sensor, isHi=isHi)

def splitSwRepStr(st):
    #<0xB1><SW1><SW2><CHK>
    b1 = st[1]
    b2 = st[2] & 0x0F
    switch = 1 + utConvertBytesToNatural(b1, b2)
    if (st[2] & bitReportClosed) == bitReportClosed:
        direction = kClosed
    else:
        direction = kThrown
    return SwRepMsg(switch = switch, direction = direction)

def splitLongAckStr(st):
    #<0xB4><lopc><ack><CHK>
    responseToOpcode = st[1] | 0x80
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
        address = utConvertBytesToNatural(st[4], st[9]),
        isAddressAlreadyInUse = (st[3] & 0x30) == 0x30,
        slot = st[2])

def splitPutTrainStateStr(st):
    #<0><1><slot#> <train state>
    return PutTrainStateMsg(slot = st[2], state = st[3])
	
def splitPutTrainPositionStr(st):
    #<0><2><slot#><count><sensor#>...<sensor#>           where sensor# is 2 bytes
    slot = st[2]
    sensorCount = st[3]
    sensors = []
    for i in range (1, sensorCount + 1):
        sensors.append(utConvertBytesToNatural(st[3 + 2 * i-1], st[3 + 2 * i]))
    return PutTrainPositionMsg(slot = slot, sensors = sensors)

def splitPutSectionStateStr(st):
    #<0><3><section#> <section state>                  where section# is 2 bytes
    return PutSectionStateMsg(
        id = utConvertBytesToNatural(st[2], st[3]),
        state = st[4])

def splitPutSwitchStateStr(st):
    #<0><4><switch#> <switch state>
    return PutSwitchStateMsg(
        id = st[2],
        state = st[3])

def splitPutSensorStateStr(st):
    #<0><5><sensor#> <sensor state>                   where sensor# is 2 bytes
    return PutSensorStateMsg(
        id = utConvertBytesToNatural(st[2], st[3]),
        state = st[4])

def splitPutInitOutcomeStr(st):
    #<0><9><physical loco address> <slot#> <virtual loco address> <slot#>   where addresses are 2 bytes
    return PutInitOutcomeMsg(
        physAdd = utConvertBytesToNatural(st[2], st[3]),
        physSlot = st[4],
        virtAdd = utConvertBytesToNatural(st[5], st[6]),
        virtSlot = st[7])

def splitPutReadLayoutResponseStr(st):
    #<0><11><XML read response flag><code>          where code is 2 bytes
    return PutReadLayoutResponseMsg(
        responseFlag = st[2],
        code = utConvertBytesToNatural(st[3], st[4]))

def splitPutTrainInformationStr(st):
    #<0><21><virtual slot#> <speed> <direction> <light> <bell> <horn> <mute>
    return PutTrainInformationMsg(
        slot = st[2], speed = st[3], direction = st[4],
        lights = st[5], bell = st[6], horn = st[7],
        mute = st[8])
			 
def splitPutPowerChangeCompleteStr(st):
    #<0><26>
    return PutPowerChangeCompleteMsg(dummy = 0)



