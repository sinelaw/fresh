# GNUmakefile syntax highlighting test
.PHONY: all clean test
CC := gcc
CFLAGS := -Wall -Wextra -O2

all: hello

hello: main.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f hello
