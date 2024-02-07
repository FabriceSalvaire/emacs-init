# Get help on...

- `describe-char` for the cursor
- `describe-face`
- `describe-function` cursor on a function
- `describe-mode` for a buffer 
- `describe-variable` cursor on a variable
- `describe-`

# Hook

Problems of using lambda in hook:
- Lambda in hook is unreadable when reading value of a hook, such as in `describe-variable` or any keybinding help or log.
- Lambda in hook cannot be removed using `remove-hook`.

