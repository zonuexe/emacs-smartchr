# emacs-smartchr

This package is an Emacs ported version of kana1's [vim-smartchr](https://github.com/kana/vim-smartchr).

## Usage

The `smartchr` function generates an interactive closure, so bind it to keymap.

```el
(global-set-key (kbd "=") (smartchr '(" = " " == " " === ")))
(define-key python-mode-map (kbd "=") (smartchr '(" = " " == ")))
```

Substitute `` `!!' `` with cursor.

```el
(global-set-key (kbd "{")
                (smartchr '("{ `!!' }" "{ \"`!!'\" }" "{")))
```

The cursor symbol can be changed with the `smartchr-template-cursor-re` variable.
