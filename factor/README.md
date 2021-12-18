Notes to self
-------------

https://factorcode.org/

Installed factor OS X 
[binary image](https://downloads.factorcode.org/releases/0.98/factor-macosx-x86-64-0.98.dmg) 
to `~/dev/factor/`

Installed emacs package `fuel` https://github.com/mcandre/fuel
`M-x factor-mode`

https://concatenative.org/wiki/view/Emacs%20Integration

Point Fuel at Factor installation

    (setq fuel-listener-factor-binary <full path to factor>)
    (setq fuel-listener-factor-image <full path to factor image>)
    
which is

    (setq fuel-listener-factor-binary "/Users/scott.bale/dev/factor/factor")
    (setq fuel-listener-factor-image "/Users/scott.bale/dev/factor/factor.image")
    
then in `.factor` file

    M-x run-factor (C-c C-z)

in listener

    IN: day10
    run-sample

prettyprint 
    
    .

misc words

    describe
    help
    &help

http://rosettacode.org/wiki/Category:Factor
