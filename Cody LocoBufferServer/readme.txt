Cody's Loco Buffer Server
~~~~~~~~~~~~~~~~~~~~~~~~~

Compiling:
Run either: Compile adaLocoLib.bat or Compile adaLocoLib (Deletes .o and .ali files).bat.
The first file listed compiles the Server and does nothing else.
The second file listed compiles the server and deletes all .o and .ali files.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Starting:
To start the server, run the startadalocolib.exe file.
The server will attempt to connect to com port 3. If no connection is made the user then have three options: enter new com port number, 0 for standalone mode or -1 to quit.

If connection to com port is made, the server will prompt for port number to start server on. You can enter 0 for the default port number of 14804 or type in a port number that you choose.
Once port number is entered, the server will start running. The write and read tasks will then start running. At this point the server will start accepting connections of TCP/IP.

If server enters standalone mode, the user will be prompted for port number. Once port number is entered, the server will start running.
When a connection sends data in standalone mode, the data is echoed back to all of the other connections. It is also echoed back to the connection that sent the message.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Running:
While the server is running the user has four options for displaying the data that is sent along the connection. You can enter a number at any time while the server is running.
0: For no data displayed
1: For data coming from just the Com Port
2: For data coming from just the TCP/IP Connections
3: All data