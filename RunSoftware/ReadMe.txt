-----------------------------------------------------------------------------------
-------------------- INSTRUCTIONS FOR RUNNING SOFTWARE ----------------------------
-----------------------------------------------------------------------------------

EXAMPLE COMMAND WINDOW SCRIPT TO RUN SOFTWARE

start RailroadBig.exe
start Throttle.exe 
start StartController.exe ip 127.0.0.1 port 1234 notrace
start AdminThrottle.exe ip 127.0.0.1 port 1235 controller layoutfile simulator.xml nokeyboardLog adminLog

PORT USAGE

      1234     SIMULATOR listens for Throttles and/or Contoller.
               Used for desktop debugging of software.

      1236     LOCOBUFFERSERVER listens for Throttles and/or Controller
               Used in lab to connect software to the Digitrax Loconet.

      1235     CONTROLLER listens for Throttles.     
     
LIST OF PROGRAMS

      GameMaker Simulator  A program that simulates the railroad in the lab; however, sensor and 
                           switch numbers don't match those in the lab. The program listens for 
                           TCP/IP clients on port 1234. These clients can be throttle programs or
                           the controller program.
                           
      LocoBufferServer     A program that provides access to the Digitrax Loconet via a USB port. 
                           The program listens for TCP/IP clients on port 1236. These clients 
                           can be throttle programs or the controller program. 
                           
      Controller:          A program which filters commands from throttles that are using a virtual
                           loco address. This is true regardless of whether or not the throttle is
                           connected to the controller or to the simulator/locobuffer server. The
                           controller does not filter commands from throttles that are using a
                           physical loco address. If you are running all throttles in standalone mode,
                           then DO NOT RUN the controller

                           
      GameMaker Throttle   A program that simulates a Digitrax UT4 throttle. It can make a TCP/IP 
                           connection to the Simulator/LocoBufferServer. In this configuration,
                           the Throttle can select or steal a locomotive using a physical or virtual
                           loco address. It can also make a TCP/IP connection to the Controller. In
                           this configuration, it can select or steal using a virtual loco address.
                           Attempting to use a physical loco address will fail.
                           
      AdminThrottle        A program that can control one or more trains. In standalone mode, the 
                           program makes a TCP/IP connection to either the simulator or the LocoBufferServer
                           and select or steals locomotives using a physical address. In controller
                           mode, the program makes a TCP/IP connection to the controller. The program
                           must first intialize the controller by providing the name of an XML file
                           that describes the track layout. Subsequently, the program must initailize
                           the locomotives using physical addresses.
             
             
COMMON SOFTWARE CONFIGURATIONS

      1) Controller connected to Simulator/LocoBufferServer.
         AdminThrottle in controller mode connected to Controller.
         GameMaker Throttles connected to the Simulator using a physical loco address.
               This allows the throttles to bypass the controller.
         GameMaker Throttles connected to the Controller or Simulator using virtual loco addresses.
               This means that all commands from the throttles will be filtered by the controller.
               
      2) No Controller.
         AdminThrottle in standalone mode connected to the Simulator/LocoBufferServer.
         GameMaker Throttles connected to the  Simulator/LocoBufferServer.
         All throttles select/steal use physical loco addresses.
             
COMMAND LINE PARAMETERS FOR PROGRAMS

GameMaker Simulator
      no parameters needed

GameMaker Throttle
      no parameters needed
      once started the program will ask for the ip address and the connection port in a popup dialog: 
         set port to simulator or locobuffer server with physical loco address to bypass the controller if there is one
         set port to simulator or locobuffer server with virtual loco address to involve the controller

Controller
      ip <address>      : use the address of the computer running the simulator/locobuffer server.
      port <number>     : use the port number of the simulator/locobuffer server
      trace/notrace     : use to display the internal behavior of the controller

AdminThrottle
      ip <address>      : use the address of the computer running the controller/simulator/locobuffer server
      port <number>     : in controller mode use the port number of the controller
                        : in standalone mode use the port number of the simulator/locobuffer server
      controller/standalone          : specify mode 
      keyboardLog/noKeyboardLog      : indicate if a keyboard log is desired
      adminLog/noAdminLog            : indicate if an adminlog is desired

LocoBufferServer
      no parameters needed
      


