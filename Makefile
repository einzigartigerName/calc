TARGET			:= calc
SOURCE_DIR		:= src/
MAIN			:= Main.hs
MODULES			:= ANSI.hs Dictionary.hs Evaluate.hs Parser.hs
CFLAGS			:= -Wall -Wextra -O2

INSTALL			:= install
INSTALL_ARGS	:= -s
PREFIX			:= /usr/local/
INSTALL_DIR		:= bin/
MAN_DIR			:= man/man1/

VERSION			:= v1.1.0

build: clean version
	@cd $(SOURCE_DIR); \
	ghc $(CFLAGS) -o $(TARGET) $(MAIN) $(MODULES)
	@mv $(SOURCE_DIR)$(TARGET) .

version:
	@sed -i '/version :: IO ()/a version = putStrLn "$(VERSION)"' $(SOURCE_DIR)$(MAIN)

clean:
	@cd $(SOURCE_DIR); \
	rm -f *.o *.hi
	@sed -i '/version = putStrLn/d' $(SOURCE_DIR)$(MAIN)

clean-all: clean
	rm -f $(TARGET)

install:
	$(INSTALL) $(INSTALL_ARGS) $(TARGET) $(PREFIX)$(INSTALL_DIR)

uninstall: clean
	rm -f $(PREFIX)$(INSTALL_DIR)$(TARGET)