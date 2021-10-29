IDIR=../include
ODIR=obj/

GITGUD= -march=native -O3  -pipe -D_GNU_SOURCE -fomit-frame-pointer
DEBUG=-O0 -Wextra -g
MEMLEAK=-ggdb  -fno-omit-frame-pointer -fsanitize=undefined
# -fomit-frame-pointer -ftree-loop-linear -ftree-vectorize
#  -fsanitize=undefined

CC=gcc
CFLAGS = $(DEBUG) -pedantic -Wall -std=c99 -lm -I$(IDIR)

DEPS=$(IDIR)/$(wildcard *.h)

SOURCES=$(wildcard *.c)

SOURCES_OBJ=$(patsubst %.c,$(ODIR)/%.o,$(SOURCES))

print-% : ; @echo $* = $($*)

$(ODIR)/%.o : %.c $(DEPS)
	@ mkdir -p obj
	$(CC) $(CFLAGS) -c -o $@ $<

program: $(SOURCES_OBJ)
	$(CC) $(CFLAGS) $(wildcard $(ODIR)/*.o) -o program

debug: $(SOURCES_OBJ) $(MY_LIBS_OBJ)
	$(CC) $(CFLAGS) $(DEBUG) $(wildcard $(ODIR)/*.o) -o program

memleak: $(SOURCES_OBJ) $(MY_LIBS_OBJ)
	$(CC) $(MEMLEAK) $(CFLAGS)  $(wildcard $(ODIR)/*.o) -o program

clean:
	rm obj -rf
	rm program
run:
	./program
gdb:
	gdb program
