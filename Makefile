TARGET			:= calc
SOURCE_DIR		:= src/
MAIN			:= Main.hs
MODULES			:= ANSI.hs Dictionary.hs Evaluate.hs Parser.hs $(HSC_OUTPUT)
HSC_MODULE		:= TermSize.hsc
HSC_OUTPUT		:= TermSize.hs
CFLAGS			:= -Wall -Wextra -O2

INSTALL			:= install
INSTALL_ARGS	:= -s
PREFIX			:= /usr/local/
INSTALL_DIR		:= bin/
MAN_DIR			:= man/man1/

VERSION			:= v2.0.1

build: clean version
	hsc2hs $(SOURCE_DIR)$(HSC_MODULE)
	@cd $(SOURCE_DIR); \
	ghc $(CFLAGS) -o $(TARGET) $(MAIN) $(MODULES)
	@mv $(SOURCE_DIR)$(TARGET) .

version:
	@sed -i '/version :: IO ()/a version = putStrLn "$(VERSION)"' $(SOURCE_DIR)$(MAIN)

clean:
	rm -f $(SOURCE_DIR)*.o $(SOURCE_DIR)*.hi
	rm -f $(SOURCE_DIR)$(HSC_OUTPUT)
	@sed -i '/version = putStrLn/d' $(SOURCE_DIR)$(MAIN)

clean-all: clean
	rm -f $(TARGET)

install:
	$(INSTALL) $(INSTALL_ARGS) $(TARGET) $(PREFIX)$(INSTALL_DIR)

uninstall: clean
	rm -f $(PREFIX)$(INSTALL_DIR)$(TARGET)
