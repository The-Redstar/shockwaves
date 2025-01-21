# Shockwaves waveform viewer library for Clash

## Viewer

The Viewer module contains the two most important classes for translation: `Display` and `Split`. `Display` determines how a value will be displayed, while `Split` determines what subsignals a type has.

`Display` can be derived for types that have `Show`, and `Split` can be derived for object that have `Generic` and only contain subtypes that implement `Split`. Automatic splitting behaviour is implemented through `AutoSplit`.

It is very important that the structure produced by `Split` is always the same, and fully defines signals even if they are not present.

## Color
Color contains the `Color` type, which can be used to define custom colors for values.

The colors `red`, `yellow`, `green`, `cyan`, `blue`, `magenta`, `white`, `gray` are made available, and a custom color can be created using the `RGB` constructor.


## Translation

Translation contains facilities for translating bitsvectors from VCD files to full value representations for the waveform viewer.