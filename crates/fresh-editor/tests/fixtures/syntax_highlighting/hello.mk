# .mk extension Makefile test
include common.mk

.PHONY: build
build:
	$(MAKE) -C src all

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@
