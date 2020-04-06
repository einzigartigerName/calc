# calc
simple interactive command line calculator.

## Arguments - calc [TERM] [ARGUMENT]
* `-d` use degree in trigonometric functions
* `-h` print help dialoge
* `-i` force interactive mode
* `-v` verbose mode: outputs token and postfix notation
* `-V` version

## Feature
Syntay: `func(..)`
* `abs`
* `tan`, `sin`, `cos`
* `atan`, `asin`, `acos`
* `log`
* `sqrt`
* `floor`
* `ceil`

## Dependencies
* ghc
* [ansi-terminal](https://github.com/feuerbach/ansi-terminal)

## Install
```bash
make
sudo make install
```

## Uninstall
```bash
sudo make uninstall
```
