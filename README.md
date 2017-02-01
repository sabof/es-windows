# es-windows
![screenshot1](https://github.com/sabof/es-windows/raw/master/screencast.gif)

This package provides user-access to internal Emacs windows, and an API for querying using it's interface. It's an extension Dimitri Fontaine's `switch-window`.

The follwing commands are available. `esw/select-window` is the core function, and the others are wrappers around it.

- esw/select-window
- esw/move-window
- esw/show-buffer
- esw/swap-two-windows
- esw/delete-window

##### Example (with 4^ input)

![screenshot1](https://github.com/sabof/es-windows/raw/master/screenshot1.png)
![screenshot2](https://github.com/sabof/es-windows/raw/master/screenshot2.png)

##### How to use

Each number represents a window. Windows followed by H or V, are internal Horizontal or Vertical splitters. The last window is an external window, showing a buffer.

First you need to select a window on which to operate, by entering it's number. RET will select it. ^, >, v, and < will split it.

It's also possible to enter an operation without specifying a window, in which case the root window will be used.

##### Integration with helm

Add the following code to your init.el:
```elisp
(require 'esw-helm)
```

From helm-find-files, helm-buffer-list, helm-mini, helm-projectile (buffers and files), use *C-c C-w* to call esw to select the target window.

[screenshot4](https://github.com/kassick/es-windows/raw/master/esw-helm-screencast.webm)
