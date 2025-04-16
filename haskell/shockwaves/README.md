# Shockwaves waveform viewer library for Clash

## Viewer

The Viewer module contains the two most important classes for translation: `Display` and `Split`. `Display` determines how a value will be displayed, while `Split` determines what subsignals a type has.

`Display` can be derived for types that have `Show`, and `Split` can be derived for object that have `Generic` and only contain subtypes that implement `Split`. Automatic splitting behaviour is implemented through `AutoSplit`.

It is very important that the structure produced by `split` matches the structure of `structure`. Signals may be left out if they are not present/defined at the time.

## Color
Color contains the `Color` type, which can be used to define custom colors for values.

The colors `red`, `yellow`, `green`, `cyan`, `blue`, `magenta`, `white`, `gray` are made available, and a custom color can be created using the `RGB` constructor.


## PostTranslation

PostTranslation contains facilities for translating bitsvectors from VCD files to full value representations for the waveform viewer.
This is part of the experimental pipeline that uses Verilator, Python scripts, and a custom version of the Clash compiler.