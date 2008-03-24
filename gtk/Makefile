all: glue.so

glue.so: glue.c
	gcc --shared glue.c -o glue.so `pkg-config gtk+-2.0 --cflags --libs`
