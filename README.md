# es-window
At the moment provides the function `esw/select-window`, which allows the selection and splitting of any window in two keypresses.  Heavily based on `switch-window` by Dimitri Fontaine.

## Instructions
When `esw/select-window` is evoked, a combinations of letters and numbers will appear in each window.

Each number represents an emacs window. Windows followed by H or V, are internal Horizontal or Vertical splitters. The last window is an external window, showing a buffer.

Type the number of the window you want, followed by RET, and that window will be used. You can also type ^, >, v, or < instead of RET, in which case the window will be split in that direction.
