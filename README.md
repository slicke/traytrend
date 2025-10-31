## This project is inactive, please see [Trndi](https://github.com/slicke/trndi) 

[![Build Status](https://travis-ci.org/slicke/traytrend.svg?branch=master)](https://travis-ci.org/slicke/traytrend)
# ![Logo](https://raw.githubusercontent.com/slicke/traytrend/master/img/up1.jpg "Logo") TrayTrend

Windows/Linux Desktop app for [NightScout](http://www.nightscout.info)!

_Main window_

![Main](https://raw.githubusercontent.com/slicke/traytrend/master/img/beta3.png "Main screen")

_Settings_

![Settings](https://raw.githubusercontent.com/slicke/traytrend/master/img/beta3_settings.png "Settings")

_Transparent movable window (optional)_

![Settings](https://raw.githubusercontent.com/slicke/traytrend/master/img/beta3_linux.png "Settings")

## Features
- Platform native program
- Displays glucose values next to the system clock in the tray bar (thus tray trend) and/or in a floating window
- GUI app can be set to be on top of other apps, if needed
- Desktop alerts/"toasts", including customizable snooze feature
- Fetches glucose values from NightScout frequently, or manually
- mmol/L and mg/dL supported
- Custom colors for high/low etc
- Run an application/command if blood glucose runs low
- Can play sounds on low/high (in testing)
- Very customizable
- Works on Windows and Linux (macOS possible in the future if there's a demand)

## Known issues
- Currently the tray icon isn't too pretty
- Read treatment data and present doses and carbs

## Get it
The Windows version is available on GitHub. Linux support is not 100% complete, there are some GUI issues but a working build exists. Feature wise both versions are identical.

## Open Source
TrayTrend is free software/open source. You are free to modify and distribute it under the GPLv3. Pull requests are very welcome if you improve something!

## Bulding TrayTrend
Since TrayTrend is written in Object Pascal, you will need a working development environment with Lazarus and FPC.
Currently [Lazarus 1.8.0](http://lazarus.freepascal.org)) is used to build TrayTrend, _lazbuild_ should also work. No additional libraries are needed.
