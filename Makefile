TARGET			:= calc
MAIN			:= Main.hs
MODULES			:= Parser.hs Dictionary.hs Evaluate.hs
CFLAGS			:= -Wall -Wextra -O2

INSTALL			:= install
INSTALL_ARGS	:= -s
PREFIX			:= /usr/local/
INSTALL_DIR		:= bin/
MAN_DIR			:= man/man1/

VERSION			:= v0.2.0

build: clean version
	ghc $(CFLAGS) -o $(TARGET) $(MAIN) $(MODULES)

version:
	@sed -i '/version :: IO ()/a version = putStrLn "$(VERSION)"' $(MAIN)

clean:
	rm -f *.o *.hi
	@sed -i '/version = putStrLn/d' $(MAIN)

clean-all: clean
	rm -f $(TARGET)

install:
	$(INSTALL) $(INSTALL_ARGS) $(TARGET) $(PREFIX)$(INSTALL_DIR)

uninstall: clean
	rm -f $(PREFIX)$(INSTALL_DIR)$(TARGET)