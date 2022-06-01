#Compiler
SRC = src
INCLUDE = include 
CC=gcc
SERVER = argusd
CLIENT = argus

#Debug or Release
DEBUG = -Og -g
RELEASE = -O3
DEBUG_MODE ?= 1
ifeq ($(DEBUG_MODE),1)
#   make DEBUG_MODE=1
    EXEC_TYPE = ${DEBUG}
else
#   make DEBUG_MODE=0
#   or just simply
#   make
    EXEC_TYPE = ${RELEASE}
endif

#Compiler options
override CFLAGS      += -Wall -Wextra -Wparentheses -Wunreachable-code -Wunused
override CFLAGS      += -Wmissing-field-initializers -Wswitch-enum
override CFLAGS      += -Wredundant-decls -Wshadow -Wswitch-default -Wuninitialized
override CFLAGS      += $(EXEC_TYPE) -I$(INCLUDE)

# server source files
SERVER_SRC = $(wildcard $(SRC)/server/*.c)
SERVER_OBJ = ${SERVER_SRC:.c=.o}

# client source files
CLIENT_SRC = $(wildcard $(SRC)/client/*.c)
CLIENT_OBJ = ${CLIENT_SRC:.c=.o}

# common source files to both apps
COMMON_SRC = $(wildcard $(SRC)/common/*.c)
COMMON_OBJ = ${COMMON_SRC:.c=.o}

.PHONY : cclean sclean clean all

# make all
all: $(SERVER) $(CLIENT)

#make engine
$(SERVER): ${SERVER_OBJ} ${COMMON_OBJ}
	$(CC) ${CFLAGS} -o $(SERVER) ${SERVER_OBJ} ${COMMON_OBJ}

#make generator
$(CLIENT): ${CLIENT_OBJ} ${COMMON_OBJ}
	$(CC) ${CFLAGS} -o $(CLIENT) ${CLIENT_OBJ} ${COMMON_OBJ}

#make clean
clean: cclean sclean

#make clean
cclean:
	rm -rf $(CLIENT_OBJ) $(COMMON_OBJ) $(CLIENT)

#make clean
sclean:
	rm -rf $(SERVER_OBJ) $(COMMON_OBJ) $(SERVER)
