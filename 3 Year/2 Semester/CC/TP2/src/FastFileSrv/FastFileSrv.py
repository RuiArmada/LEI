import socket

import sys  # We use this module to get the arguments sent via the command line and to add the parent directory to the program's path
sys.path.append("../")
from Packet import Packet, PacketType, CHUNK_SIZE

# Gateway's address
HOST = sys.argv[1]
PORT = int(sys.argv[2])

# Server's socket
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

# Estabilishing a connection with the gateway.
s.sendto(Packet(PacketType.NEW_CONNECTION, b'').serialize(), (HOST,PORT))
s.recvfrom(CHUNK_SIZE)
print("Connected to gateway with sucess!")

# Main server loop, waits for file requests and sends the appropriate chunks.
while True:

    # Initializing variables
    f = None        # file descriptor
    filename = None # file name
    chunk = 0       # chunk counter

    # Waits for a file request
    p, _ = s.recvfrom(CHUNK_SIZE * 2)
    packet = Packet.deserialize(p)
    if packet.type == PacketType.REQUEST_FILE:

        # Extract the filename (fx) and client (address of the machine who requested the file) from the packet's data
        fx, client = str(packet.data,"utf-8").rsplit()
        cx = packet.chunkN # Requested chunk

        print(f"Received request for chunk {cx} of file {fx}.")

        try:

            # If the currently opened file is too far ahead or if the opened file is not the correct one...
            if filename != fx or chunk > cx: 
                if f: f.close()         # we close the current file, is there is an opened file...
                filename = fx           # we specify the new file's name...
                f = open(filename,"rb") # we open the new file...
                chunk = 0               # and we reset the chunk counter

            # We read chunks from the file until we get to the "right" one
            while chunk < cx:
                f.read(CHUNK_SIZE - 16)
                chunk += 1

            # We read the chunk we want to send
            fc = f.read(CHUNK_SIZE - 16)
            chunk += 1
    
            # And we send the chunk, placing it in a packet along with the client's address (this is needed for the demultiplexer)
            # 'len(fc) == CHUNK_SIZE - 16' tells us if this chunk is the last one
            s.sendto(Packet(PacketType.FILE_CHUNK, client.ljust(16).encode() + fc, cx, len(fc) == CHUNK_SIZE - 16).serialize(), (HOST,PORT))
            print(f"Packet with chunk {cx} sent successfully.")

        except FileNotFoundError: # If the server cannot open the requested file
            print(f"ERROR - file {fx} not found.")
            s.sendto(Packet(PacketType.FILE_NOT_FOUND_ERROR, bytes()).serialize(), (HOST,PORT))
    
    else: # If the packet is not a file request packet, something went wrong.
        print("ERROR - invalid packet type received.")

