with TcpIp; use TcpIp;
package GlobalAdmin is
	socket : socketType := zeroSocket;  
	-- Connects the AdminThrottle to either the simulator/locobuffer or to the controller.
	-- Should be connected in blocking mode.
end GlobalAdmin;