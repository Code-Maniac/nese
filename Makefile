.PHONY: clean install full
CC = g++
CFLAGS = -g -Wall -std=c++1y
LCTH = -lcth -lGLEW -lGL -lSDL2 -lSDL2main -lSDL2_image -lSDL2_mixer -lboost_filesystem -lboost_system -lyaml-cpp -lIL -lILU  -lassimp -llua -ldl -lfreetype -lboost_program_options
LFLAGS = $(LCTH)
OBJ_FILES = main.o \
	    cpu_6502.o

all: nese

obj/%.o: %.cpp
	mkdir -p obj
	$(CC) $(CFLAGS) -c -o $@ $<

nese: $(addprefix obj/,$(notdir $(OBJ_FILES)))
	$(CC) $(CFLAGS) -o $@ $+ $(LFLAGS)

clean:
	rm -f obj/*.o core.* nese

install:
	cp nese /usr/local/bin/nese

full:
	make clean && make -j10 && make install
