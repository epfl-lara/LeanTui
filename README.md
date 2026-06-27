# LeanTui

LeanTui is a small Lean 4 library for text-based user interfaces. It models a
terminal screen as a virtual buffer, renders ANSI escape sequences, and emits
incremental updates by diffing the previous screen against the next one.

The first version is intentionally self-contained: it depends only on Lean's
standard `IO` facilities and writes strings to the terminal.

## Pieces

- `LeanTui.Ansi`: escape sequence helpers.
- `LeanTui.Style`: basic colors and text attributes.
- `LeanTui.Screen`: virtual screen buffers and diff rendering.
- `LeanTui.Key`: keyboard command decoding.
- `LeanTui.Terminal`: terminal IO helpers.
- `LeanTui.App`: a simple `init`/`view`/`update` event loop.

## Demos

### screen-edit-demo

```bash
lake exe screen-edit-demo README.md
```

  * move with cursor keys
  * edit screen content
  * exit using ESC key

The demo enters the terminal's alternate screen and tries to switch the
controlling terminal into raw no-echo mode with `stty -f /dev/tty` on macOS,
falling back to plain `stty`. Cursor keys and `q` should be read as single-key
commands. `Terminal.readKeyLine` is also available for tests or environments
where raw terminal input is not desirable.

Raw input is parsed through a small buffered reader. If the terminal delivers
several cursor-key escape sequences in one read, as can happen with key repeat,
the reader consumes one key and keeps the remaining bytes for subsequent events.

The app loop is frame-driven. It polls raw input without blocking, applies all
currently available key events to the model, renders one diff frame, and then
sleeps until the next frame. Set `frameDelayMs := 0` on an app to disable frame
pacing.

### chess-demo

```bash
lake exe chess-demo
```

  * move the cursor with arrow keys
  * select and place pieces with Enter or Space
  * reset the board with `r`
  * exit using `q` or ESC

The chess demo renders pieces with Unicode chess symbols. It enforces turn order,
ordinary piece movement, captures, castling, en passant, and automatic queen
promotion. Check is intentionally omitted to keep the example compact.
