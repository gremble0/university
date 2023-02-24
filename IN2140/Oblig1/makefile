$CFLAGS = -g -Wall -Wextra -std=gnu11

all: vowelshift stringoperations apple

vowelshift: vowelshift.c
	gcc vowelshift.c -o vowelshift $(CFLAGS)

stringoperations: stringoperations.c
	gcc tests.c stringoperations.c -o stringoperations $(CFLAGS)

apple: apple.c
	gcc apple.c the_apple.c -o apple $(CFLAGS)

clean:
	rm -f *.exe vowelshift stringoperations apple
