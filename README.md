# calc
simple interactive command line calculator.

## Arguments - calc [TERM] [ARGUMENT]
* `-d` use degree in trigonometric functions
* `-h` print help dialoge
* `-i` force interactive mode
* `-v` verbose mode: outputs token and postfix notation
* `-V` version

## Supported Functions
Syntax: `func(..)`
* `abs`
* `tan`, `sin`, `cos`
* `atan`, `asin`, `acos`
* `log`
* `exp`
* `sqrt`
* `floor`
* `ceil`

## Input History
Use the up and down arrow keys so scroll through your sessions input history

## Dependencies
* ghc

## Install
```bash
make
sudo make install
```

## Uninstall
```bash
sudo make uninstall
```
