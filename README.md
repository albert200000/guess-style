# guess-style

## What's this for?
Guess variables like c-basic-offset, tab-width and indent-tabs-mode automatically.

## Configuration
Add the following to your .emacs:

```elisp
(load "/path/to/guess-style")
(add-hook 'prog-mode-hook 'guess-style-guess-all)
```
## Usage
To guess variables when a major mode is loaded, add guess-style-guess-all to that mode's hook like this: (add-hook 'c-mode-common-hook 'guess-style-guess-all)

To change what variables are guessed, customize guess-style-guesser-alist.

To show some of the guessed variables in the mode-line, enable guess-style-info-mode. You can do this by adding this to your .emacs:
```elisp
(global-guess-style-info-mode 1)
```
