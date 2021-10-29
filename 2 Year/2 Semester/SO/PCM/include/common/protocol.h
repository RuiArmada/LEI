#ifndef SO_PROJETO_PROTOCOL_H
#define SO_PROJETO_PROTOCOL_H

#define CLIENTPIPES_LEN 39
#define SERVER_ACK "Ready for input"
#define SERVER_ACK_LEN sizeof(SERVER_ACK)
#define CLOSE "\x90"
#define WRITE_LITERAL(fd, str) write(fd, str, sizeof(str))
#define END_OF_MESSAGE(fd) write(fd, "", sizeof(""))

#endif //SO_PROJETO_PROTOCOL_H  
