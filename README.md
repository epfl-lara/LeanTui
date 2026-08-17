# LeanTui

LeanTui is a small Lean 4 library for text-based user interfaces. It models a
terminal screen as a virtual buffer, renders ANSI escape sequences, and emits
incremental updates by diffing the previous screen against the next one.

The first version is intentionally self-contained: it depends only on Lean's
standard `IO` facilities and writes strings to the terminal.

### Literate source documentation

Build complete, syntax-highlighted source documentation for the ScreenEdit demo and its LeanTui dependency with Verso:

```bash
./scripts/build-literate-docs.sh
```

The ScreenEdit source page is generated at
`literatedocbuild/.lake/build/literate-html/Demo/ScreenEdit/ScreenEditDemo/index.html`.
Serve the generated site locally with:

```bash
python3 -m http.server 8000 \
  -d literatedocbuild/.lake/build/literate-html
```

Then open <http://localhost:8000/Demo/ScreenEdit/ScreenEditDemo/>.

Verso is isolated in `literatedocbuild` because it uses Lean `v4.33.0`, while
the `doc-gen4` build remains pinned to Lean `v4.29.0`. The build also applies a
local CSS override so long source files scroll correctly in a fixed-height
browser window.

## Pieces

- `LeanTui.Ansi`: escape sequence helpers.
- `LeanTui.Style`: basic colors and text attributes.
- `LeanTui.Screen`: virtual screen buffers and diff rendering.
- `LeanTui.Key`: keyboard command decoding.
- `LeanTui.Terminal`: terminal IO helpers.
- `LeanTui.App`: a simple `init`/`view`/`update` event loop.

## Terminal Behavior

LeanTui apps enter the terminal's alternate screen and try to switch the
controlling terminal into raw no-echo mode. Raw input is parsed through a small
buffered reader. If the terminal delivers several key or mouse escape sequences
in one read, as can happen with key repeat, the reader decodes the available
batch before the app renders the next frame.

The app loop is input-driven. It waits until a keyboard or mouse event is
available, applies the available events to the model, checks the terminal size,
and renders one diff frame. This keeps idle sessions quiet; terminal resizes are
picked up after the next input event rather than immediately.

`Terminal.readKeyLine` is also available for tests or environments where raw
terminal input is not desirable.

## Instrumentation

Apps can observe terminal output volume with the `terminalOutput` hook. LeanTui
maintains a cumulative count of characters written by the app runner and passes
that count into the model before each render, making it available for lightweight
performance displays.

For runtime sharing diagnostics, enable `LeanTui.App.SharedReferenceTrace` in
`LeanTui/App.lean`:

```lean
def enabled : Bool := true
```

Then rebuild and redirect stderr to a log file while running an app:

```bash
lake build chess-demo
lake exe chess-demo 2> shared.log
```

The log receives timestamped start/end markers and any `dbgTraceIfShared`
messages for persistent app-loop values such as the model and screen buffers.
The TUI itself still renders to stdout.

## SSH and Remote Terminals

LeanTui is expected to work over SSH when the remote process has an allocated
pseudo-terminal and the local terminal understands common ANSI/xterm escape
sequences. Prefer running interactive apps through `ssh -t`, for example:

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
resize checks run after input events instead of on an idle timer, which keeps SSH
sessions from spawning `stty size` while nothing is happening.

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

## Demos

### screen-edit-demo

```bash
lake exe screen-edit-demo README.md
```

  * move with cursor keys
  * edit screen content
  * exit using ESC key

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
