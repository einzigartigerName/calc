TARGET		:= calc
SOURCE		:= Main.hs Parser.hs Dictionary.hs
CFLAGS		:= -Wall -Wextra -O2

build: clean
	ghc $(CFLAGS) -o $(TARGET) $(SOURCE)
	strip --strip-unneeded $(TARGET)

clean:
	rm -f *.o *.hi

clean-all: clean
	rm -f $(TARGET)
