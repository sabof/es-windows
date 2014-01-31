# es-windows
This package allows "advanced" layout manipulations with three key presses. In an arbitrary layout, it can swap two windows, make a window occupy the full side of a frame, and perform other operations which are impractical using standard commands. It also provides a conveninet API for creating interactive layout manipulation commands. The interface is based on Dimitri Fontaine's `switch-window`

The follwing commands are available:

- esw/select-window
- esw/move-window
- esw/show-buffer
- esw/swap-two-windows
- esw/delete-window

## esw/select-window
This is the core function. The other functions are wrappers.

##### Example (with 4^ input)

<!-- FIXME: Change to a single gif -->
![screenshot1](https://github.com/sabof/es-windows/raw/master/screenshot1.png)
![screenshot2](https://github.com/sabof/es-windows/raw/master/screenshot2.png)

##### Instructions

Each number represents an emacs window. Windows followed by H or V, are
internal Horizontal or Vertical splitters. The last window is an external
window, showing a buffer.

Type the number of the window you want, followed by RET, and that window will be
used. You can also type ^, >, v, or < instead of RET, in which case the window
will be split in that direction.

If no window is provided, the closest to root window will be used.
