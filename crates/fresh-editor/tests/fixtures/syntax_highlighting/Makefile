# Makefile syntax highlighting test
CC = gcc
CFLAGS = -Wall -O2
TARGET = hello

.PHONY: all clean

all: $(TARGET)

$(TARGET): main.o utils.o
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) *.o
