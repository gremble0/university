CFLAGS = -std=gnu11 -g -Wall -Wextra
VFLAGS = --leak-check=full --show-leak-kinds=all --track-origins=yes --malloc-fill=0x40 --free-fill=0x23
BIN = ./load_example1/load_fs ./load_example2/load_fs ./load_example3/load_fs \
	./create_example1/create_fs ./create_example2/create_fs ./create_example3/create_fs

#creates the programs
all: $(BIN)

create_example%/create_fs: allocation.o inode.o create_example%/create_fs.o
	gcc $(CFLAGS) $^ -o $@ -lm

load_example%/load_fs: allocation.o inode.o load_example%/load_fs.o
	gcc $(CFLAGS) $^ -o $@ -lm

%.o: %.c
	gcc $(CFLAGS) -c -I. $^ -o $@


#run 
run_load1:
	cd load_example1 && \
	./load_fs

run_load2:
	cd load_example2 && \
	./load_fs


run_load3:
	cd load_example3 && \
	./load_fs


run_create1:
	cd create_example1 && \
	./create_fs


run_create2:
	cd create_example2 && \
	./create_fs


run_create3:
	cd create_example3 && \
	./create_fs


#valgrind mem check
valgrind_load1:
	cd load_example1 && \
	valgrind $(VFLAGS) ./load_fs


valgrind_load2:
	cd load_example2 && \
	valgrind $(VFLAGS) ./load_fs


valgrind_load3:
	cd load_example3 && \
	valgrind $(VFLAGS) ./load_fs


valgrind_create1:
	cd create_example1 && \
	valgrind $(VFLAGS) ./create_fs


valgrind_create2:
	cd create_example2 && \
	valgrind $(VFLAGS) ./create_fs


valgrind_create3:
	cd create_example3 && \
	valgrind $(VFLAGS) ./create_fs



clean:
	rm -rf *.o
	rm -f $(BIN)
