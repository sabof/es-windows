# es-windows
At the moment provides the functions `esw/select-window`, `esw/move-window` and `esw/show-buffer`, which allow the selection and splitting of any window (including internal windows) in two keypresses. Based on `switch-window` by Dimitri Fontaine.

![screenshot1](https://github.com/sabof/es-windows/raw/master/screenshot1.png)
![screenshot2](https://github.com/sabof/es-windows/raw/master/screenshot2.png)

### Instructions

Each number represents an emacs window. Windows followed by H or V, are
internal Horizontal or Vertical splitters. The last window is an external
window, showing a buffer.

Type the number of the window you want, followed by RET, and that window will be
used. You can also type ^, >, v, or < instead of RET, in which case the window
will be split in that direction.

If no window is provided, the closest to root window will be used.
