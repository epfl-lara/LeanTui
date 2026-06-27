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
  * click a board square in terminals that support SGR mouse reporting
  * select and place pieces with Enter or Space
  * reset the board with `r`
  * exit using `q` or ESC

The chess demo renders pieces with Unicode chess symbols. It enforces turn order,
ordinary piece movement, captures, castling, en passant, and automatic queen
promotion. Check is intentionally omitted to keep the example compact.

## SSH and remote terminals

LeanTui is expected to work over SSH when the remote process has an allocated
pseudo-terminal and the local terminal understands common ANSI/xterm escape
sequences. Prefer running interactive demos through `ssh -t`, for example:

```bash
ssh -t host 'cd /path/to/leanedit && lake exe chess-demo'
```

The library currently assumes:

  * `stty` is available on the remote host for raw mode and terminal size
    detection
  * the remote session has a real TTY, not plain piped stdin/stdout
  * the local terminal supports ANSI cursor movement, alternate screen mode, and
    UTF-8 text
  * mouse input, when used, is passed through by the local terminal and SSH
    client as SGR mouse escape sequences

LeanTui tries both BSD/macOS (`stty -f /dev/tty`) and GNU/Linux
(`stty -F /dev/tty`) forms before falling back to inherited stdin. Terminal
resize checks are throttled instead of running every frame, which keeps SSH
sessions from spawning `stty size` excessively while still redrawing after a
resize.

Known remedies for less capable SSH setups:

  * use `ssh -t` or configure the SSH client to allocate a TTY
  * use a UTF-8 locale such as `LANG=en_US.UTF-8`
  * disable mouse-dependent interactions and keep keyboard controls available
  * fall back to `Terminal.readKeyLine` or a custom `readKey?` reader in
    environments where raw input is not available
  * avoid `$TERM=dumb`; use a terminal type compatible with xterm-style ANSI
    controls when possible

Future hardening work should make these assumptions more explicit at runtime:
detect raw-mode or size-detection failures, add true UTF-8 input decoding, and
recognize more terminal-specific key escape variants.
