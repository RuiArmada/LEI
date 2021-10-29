from __future__ import annotations  # Allows us to specify the functions' return type as an annotation
from enum import Enum               # We use this class as a superclass to our PacketType enum.
import pickle                       # We use this module to serialize and deserialize our packets.
import hashlib                      # We use this module to hash the packets' data.

CHUNK_SIZE = 512   # Size of a chunk. This value is arbitrary, but shouldn't be too big or too small.

class PacketType(Enum):
    REQUEST_FILE = 0            # Request for a chunk of a file, sent by the gateway
    FILE_CHUNK = 1              # Chunk of a file, sent by a server
    FILE_NOT_FOUND_ERROR = 2    # Sent by a server if it doesn't have the specified file
    NEW_CONNECTION = 3          # Sent by a server to the gateway when it starts running

class Packet:
    """
    Class used to represent a packet. 
    
    This type of packet allows us to use the FS Chunk Protocol to send and receive files over UDP connections.

    ...

    Attributes:
    -----------
    type : PacketType
        specifies the packet's type
    data : bytes
        the raw data being sent/received
    chunkN : int
        if a packet contains a chunk of a file, this attribute tells us which chunk it is (default 0)
    hasNext : bool
        if a packet contains a chunk of a file, this attribute tells us if it is the last chunk (default False)
    md5 : bytes
        MD5 hash of the packet's data, used to check data integrity
    """

    def __init__(self, type : PacketType, data : bytes, chunkN : int = 0, hasNext : bool = False) -> Packet:
        self.type = type
        self.chunkN = chunkN
        self.data = data
        self.hasNext = hasNext
        self.md5 = hashlib.md5(data).digest()

    def serialize(self) -> bytes:
        """Converts an instance of Packet to a byte array, allowing it to be sent over a socket."""
        return pickle.dumps(self)

    @staticmethod
    def deserialize(b) -> Packet:
        """Converts a byte array to an instance of Packet, making it readable after being received through a socket."""
        return pickle.loads(b)

