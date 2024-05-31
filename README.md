# spot-on
Spotify plugin for Polybar based on [PrayagS/polybar-spotify](https://github.com/PrayagS/polybar-spotify).

[<img src="scroll.gif" width="300"/>](scroll.gif)

### Features
  - Comes in a standalone binary
  - Light-weight
  - Updates asynchronously instead of polling Spotify constantly
  - Text scrolling
  - Written in Haskell

# Installation
You can find the `spot-on` binary in [releases](https://github.com/MiksuR/spot-on/releases).

Include the following modules in your `config.ini`:
```ini
[module/spot-on]
type = custom/script
tail = true
interval = 0.5
format-prefix = "<spotify-logo> "
format = <label>
format-padding = 1
exec = bash -c "~/.config/polybar/spot-on scroll & wait"

[module/spot-on-previous]
type = custom/script
exec = echo "<previous-icon>"
format = <label>
format-padding = 1
click-left = ~/.config/polybar/spot-on previous

[module/spot-on-playpause]
type = custom/ipc
hook-0 = echo "<play-icon>"
hook-1 = echo "<pause-icon>"
format-padding = 1
initial = 1
click-left = ~/.config/polybar/spot-on playpause

[module/spot-on-next]
type = custom/script
exec = echo "<next-icon>"
format = <label>
format-padding = 1
click-left = ~/.config/polybar/spot-on next
```
