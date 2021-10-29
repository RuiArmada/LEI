from aiohttp import web
import threading

from Packet import Packet, PacketType, CHUNK_SIZE
import hashlib
import socket   
import sys

GW_IP = sys.argv[1] if len(sys.argv) > 1 else '0.0.0.0'    # Address used by the gateway to communicate with the file servers/clients
PORT = int(sys.argv[2]) if len(sys.argv) > 2 else 80       # and the respective port
                                                           # These can be changed, but it is not necessary

# Initializing variables and locks
# The locks are used to avoid race conditions and other problems resulting from the use of more than one thread
# The condition is used to let the HTTP request handler know that the chunk it requested may have arrived
known_hosts = dict()
hosts_lock = threading.RLock()
file_chunks = dict()
files_lock = threading.RLock()
files_cond = threading.Condition(files_lock)

# The demultiplexer receives every UDP packet sent to the gateway and performs the required actions for each
def demultiplexer():
    while True:

        try:
            p, addr = s.recvfrom(CHUNK_SIZE * 2)
        except ConnectionResetError: # If one of the servers is unreachable, continue. This will be dealt with in another part of the program.
            continue

        packet : Packet = Packet.deserialize(p)
        print(f"Received packet from server {addr}.")

        if packet.type == PacketType.NEW_CONNECTION:
            with hosts_lock:
                known_hosts[addr] = 0
            print(f"Added server {addr} to list of known hosts.")
            s.sendto(b'',addr)

        else:
            if addr not in known_hosts:
                print(f"ERROR - Received packet from unknown server {addr}.")
            elif packet.type == PacketType.FILE_CHUNK:
                with files_lock:
                    if packet.md5 == hashlib.md5(packet.data).digest():
                        file_chunks[str(packet.data[:16],"utf-8").strip()] = (packet.data[16:],addr,packet.hasNext)
                        files_cond.notify_all()
            elif packet.type == PacketType.FILE_NOT_FOUND_ERROR:
                with hosts_lock:
                    known_hosts.pop(addr)
                print(f"ERROR - Server {addr} does not contain the requested file(s) - removing it from list of known hosts.")



routes = web.RouteTableDef()

@routes.get('/{name}')
async def handler(request : web.Request):
    filename = request.match_info['name']
    client = request.remote
    print(client)
    
    print(f"Request for {filename} received.")

    chunk = 0
    f = bytes()

    while chunk >= 0:
        if len(known_hosts) == 0:
            print(f"ERROR - could not get file {filename} - aborting!")
            return web.Response(status=404)
        for host in known_hosts.copy():
            s.sendto(Packet(PacketType.REQUEST_FILE, filename.ljust(256).encode() + client.encode(),chunk).serialize(), host)
            while True:
                with files_lock:
                    files_cond.wait_for(lambda: client in file_chunks, 0.5)
                    if client in file_chunks:
                        data, addr, hasNext = file_chunks.pop(client,(None,None,None))
                        if addr == host:
                            break
                    else:
                        with hosts_lock:
                            if host in known_hosts:
                                known_hosts[host] += 1
                                if known_hosts[host] > 4:
                                    known_hosts.pop(host)
                                    print(f"Server {host} timed out too many times - removing it from list of known hosts.")
                        data = None
                        break
            if data:
                f += data
                if hasNext:
                    chunk += 1
                else:
                    chunk = -1
                    break
    return web.Response(body=f)

app = web.Application()
app.add_routes(routes)

if __name__ == '__main__':
    with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
        s.bind((GW_IP,PORT))

        t = threading.Thread(target=demultiplexer, daemon=True)
        t.start()
        web.run_app(app, host=GW_IP, port=PORT)
