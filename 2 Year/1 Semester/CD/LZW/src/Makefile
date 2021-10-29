#OPT = -flto -Ofast -march=native -mtune=native
CFLAGS = -lm -Wno-pointer-sign -pedantic-errors $(OPT)
#CFLAGS = -g
CC = gcc

all: load.o lzwd.o
	$(CC) $(CFLAGS) -o lzwd $^

clean:
	@rm -rf *.o c t f 
