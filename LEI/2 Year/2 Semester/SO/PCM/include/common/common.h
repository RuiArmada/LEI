#ifndef SO_COMMON_H
#define SO_COMMON_H

ssize_t readMsg(int fildes, char *buf, size_t nbyte);
ssize_t readln(int fildes, char *buf, size_t nbyte);
int countNumberOfChars(long long n);

#endif //SO_COMMON_H 
