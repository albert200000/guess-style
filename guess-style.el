;;; guess-style.el --- automatic setting of code style variables -*- lexical-binding:t -*-

(require 'cl-lib)

(add-to-list 'debug-ignored-errors "^Not enough lines to guess variable$")
(add-to-list 'debug-ignored-errors "^Not certain enough to guess variable$")

(defgroup guess-style nil
  "Automatic setting of code style variables."
  :group 'files
  :group 'languages)

(defcustom guess-style-output-messages nil
  "Whether to output messages"
  :group 'guess-style
  :type 'boolean)

(defcustom guess-style-guesser-alist
  `((indent-tabs-mode . (guess-style-guess-tabs-mode nil))
    (tab-width . (guess-style-guess-tab-width nil))
    (c-basic-offset . (guess-style-guess-indent cc-mode))
    (nxml-child-indent . (guess-style-guess-indent xml-mode))
    (css-indent-offset . (guess-style-guess-indent css-mode))
    (css-indent-offset . (guess-style-guess-indent scss-mode))
    (standard-indent . (guess-style-guess-indent web-mode))
    (web-mode-markup-indent-offset . (guess-style-guess-indent web-mode))
    (web-mode-code-indent-offset . (guess-style-guess-indent web-mode))
    (web-mode-attr-indent-offset . (guess-style-guess-indent web-mode))
    (js-indent-level . (guess-style-guess-indent js-mode))
    (js-indent-level . (guess-style-guess-indent js-jsx-mode))
    (js-indent-level . (guess-style-guess-indent js-ts-mode))
    (js-jsx-indent-level . (guess-style-guess-indent js-jsx-mode))
    (js-switch-indent-offset . (guess-style-guess-indent js-mode))
    (json-mode-indent-level . (guess-style-guess-indent json-mode))
    (js2-basic-offset . (guess-style-guess-indent js2-mode))
    (lua-indent-level . (guess-style-guess-indent lua-mode))
    (perl-indent-level . (guess-style-guess-indent perl-mode))
    (cperl-indent-level . (guess-style-guess-indent cperl-mode))
    (raku-indent-offset . (guess-style-guess-indent raku-mode))
    (erlang-indent-level . (guess-style-guess-indent erlang-mode))
    (ada-indent . (guess-style-guess-indent ada-mode))
    (sgml-basic-offset . (guess-style-guess-indent sgml-mode))
    (pascal-indent-level . (guess-style-guess-indent pascal-mode))
    (typescript-indent-level . (guess-style-guess-indent typescript-mode))
    (sh-basic-offset . (guess-style-guess-indent sh-mode))
    (ruby-indent-level . (guess-style-guess-indent ruby-mode))
    (enh-ruby-indent-level . (guess-style-guess-indent enh-ruby-mode))
    (crystal-indent-level . (guess-style-guess-indent crystal-mode))
    (rust-indent-offset . (guess-style-guess-indent rust-mode))
    (rustic-indent-offset . (guess-style-guess-indent rustic-mode))
    (scala-indent:step . (guess-style-guess-indent scala-mode))
    (pug-tab-width . (guess-style-guess-indent pug-mode)))
  "*A list of cons containing a variable and a list with guesser function and major mode."
  :group 'guess-style
  :type '(repeat (cons variable (function symbol))))

(defun guess-style-guess-variable (variable guesser mode)
  "Guess a value for VARIABLE according to `guess-style-guesser-alist'.
If GUESSER is set, it's used instead of the default."
  (when (or (null(car mode)) (eq (car mode) major-mode))
    (progn
      (unless guesser
        (setq guesser (car(cdr (assoc variable guess-style-guesser-alist)))))
      (condition-case err
          (set (make-local-variable variable)
               (funcall guesser))
        (when guess-style-output-messages
          (message "%s variable Guessed (%s)"
                   variable (symbol-value variable))
          `(lambda () ,(symbol-value variable)))
        (error (when guess-style-output-messages
                 (message "Could not guess variable '%s' (%s)" variable (error-message-string err)))
               `(lambda () (error "%s" (error-message-string ,err))))))))

;;;###autoload
(defun guess-style-guess-all ()
  "Guess all variables in `guess-style-guesser-alist'.
Special care is taken so no guesser is called twice."
  (interactive)
  (let (cache match)
    (dolist (pair guess-style-guesser-alist)
      ;; Cache, so we don't call the same guesser twice.
      (if (setq match (assoc (car(cdr pair)) cache))
          (guess-style-guess-variable (car pair) (car(cdr match)) (cdr(cdr pair)))
        (push (cons (car(cdr pair))
                    (guess-style-guess-variable (car pair) (car(cdr pair)) (cdr(cdr pair))))
              cache)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom guess-style-maximum-false-spaces 0.3
  "*The percentage of space indents with five or more to keep `tab-width' at 4."
  :type 'number
  :group 'guess-style)

(defcustom guess-style-minimum-line-count 3
  "*The number of significant lines needed to make a guess."
  :type 'number
  :group 'guess-style)

(defcustom guess-style-too-close-to-call .05
  "*Certainty Threshold under which no decision will be made."
  :type 'number
  :group 'guess-style)

(defcustom guess-style-maximum-false-tabs 0.3
  "*The percentage of tab lines allowed to keep `indent-tabs-mode' nil."
  :type 'number
  :group 'guess-style)

(defcustom guess-style-maximum-false-indent 0.1
  "*Percentage of lines indented at a lower value than the default."
  :type 'number
  :group 'guess-style)

(defun guess-style-guess-tabs-mode ()
  "Guess whether tabs are used for indenting in the current buffer."
  (save-restriction
    (widen)
    (let* ((num-tabs (how-many "^\t" (point-min) (point-max)))
           (num-nontabs (how-many "^    " (point-min) (point-max)))
           (total (+ num-tabs num-nontabs)))
      (when (< total guess-style-minimum-line-count)
        (error "Not enough lines to guess variable"))
      (> (/ (float num-tabs) total) guess-style-maximum-false-tabs))))

(defun guess-style-guess-tab-width ()
  "Guess whether \\t in the current buffer is supposed to mean 4 or 8 spaces."
  (save-restriction
    (widen)
    (let ((many-spaces (how-many "^\t+ \\{4,7\\}[^ ]" (point-min) (point-max)))
          (few-spaces (how-many "^\t+  ? ?[^ ]" (point-min) (point-max))))
      (when (< (+ many-spaces few-spaces) guess-style-minimum-line-count)
        (error "Not enough lines to guess variable"))
      (if (> many-spaces
             (* guess-style-maximum-false-spaces few-spaces)) 8 4))))

(defun guess-style-guess-indent ()
  (let* ((tab (cl-case tab-width
                (8 "\\(\\( \\{,7\\}\t\\)\\|        \\)")
                (4 "\\(\\( \\{,3\\}\t\\)\\|    \\)")
                (2 "\\(\\( ?\t\\)\\|  \\)")))
         (end "[^[:space:]]")
         (two-exp (cl-case tab-width
                    (8 (concat "^" tab "*   \\{4\\}?" end))
                    (4 (concat "^" tab "*  " end))
                    (2 (concat "^" tab tab "\\{2\\}*" end))))
         (four-exp (cl-case tab-width
                     (8 (concat "^" tab "* \\{4\\}" end))
                     (4 (concat "^" tab tab "\\{2\\}*" end))
                     (2 (concat "^" tab "\\{2\\}" tab "\\{4\\}*" end))))
         (eight-exp (cl-case tab-width
                      (8 (concat "^" tab "+" end))
                      (4 (concat "^" tab "\\{2\\}+" end))
                      (2 (concat "^" tab "\\{4\\}+" end))))
         (two (how-many two-exp))
         (four (how-many four-exp))
         (eight (how-many eight-exp))
         (total (+ two four eight))
         (too-close-to-call (* guess-style-too-close-to-call total)))
    (when (< total guess-style-minimum-line-count)
      (error "Not enough lines to guess variable"))
    (let ((two-four (- two (* guess-style-maximum-false-indent four)))
          (two-eight (- two (* guess-style-maximum-false-indent eight)))
          (four-eight (- four (* guess-style-maximum-false-indent eight))))
      (or (if (> two-four 0)
              (if (> two-eight 0)
                  (unless (< (min two-four two-eight) too-close-to-call) 2)
                (unless (< (min two-four (- two-eight)) too-close-to-call) 8))
            (if (> four-eight 0)
                (unless (< (min (- two-four) four-eight) too-close-to-call) 4)
              (unless (< (- four-eight) too-close-to-call) 8)))
          (error "Not certain enough to guess variable")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun guess-style-get-indent ()
  (interactive)
  (message "%d"
           (cl-case major-mode
             (nxml-mode (when (boundp 'nxml-child-indent) nxml-child-indent))
             (css-mode (when (boundp 'css-indent-offset) css-indent-offset))
             (scss-mode (when (boundp 'css-indent-offset) css-indent-offset))
             (web-mode (when (boundp 'web-mode-markup-indent-offset) web-mode-markup-indent-offset))
             (js-mode (when (boundp 'js-indent-level) js-indent-level))
             (js-ts-mode (when (boundp 'js-indent-level) js-indent-level))
             (js-jsx-mode (when (boundp 'js-jsx-indent-level) js-jsx-indent-level))
             (json-mode (when (boundp 'json-mode-indent-level) json-mode-indent-level))
             (js2-mode (when (boundp 'js2-basic-offset) js2-basic-offset))
             (lua-mode (when (boundp 'lua-indent-level) lua-indent-level))
             (perl-mode (when (boundp 'perl-indent-level) perl-indent-level))
             (cperl-mode (when (boundp 'cperl-indent-level) cperl-indent-level))
             (raku-mode (when (boundp 'raku-indent-offset) raku-indent-offset))
             (erlang-mode (when (boundp 'erlang-indent-level) erlang-indent-level))
             (ada-mode (when (boundp 'ada-indent) ada-indent))
             (sgml-mode (when (boundp 'sgml-basic-offset) sgml-basic-offset))
             (pascal-mode (when (boundp 'pascal-indent-level) pascal-indent-level))
             (typescript-mode (when (boundp 'typescript-indent-level) typescript-indent-level))
             (sh-mode (when (boundp 'sh-basic-offset) sh-basic-offset))
             (ruby-mode (when (boundp 'ruby-indent-level) ruby-indent-level))
             (enh-ruby-mode (when (boundp 'enh-ruby-indent-level) enh-ruby-indent-level))
             (crystal-mode (when (boundp 'crystal-indent-level) crystal-indent-level))
             (rust-mode (when (boundp 'rust-indent-offset) rust-indent-offset))
             (rustic-mode (when (boundp 'rustic-indent-offset) rustic-indent-offset))
             (scala-mode (when (boundp 'scala-indent:step) scala-indent:step))
             (pug-mode (when (boundp 'pug-tab-width) pug-tab-width))
             (otherwise (if
                            (and (boundp 'c-buffer-is-cc-mode)
                                 c-buffer-is-cc-mode
                                 (boundp 'c-basic-offset))
                            c-basic-offset
                          tab-width)))))

(provide 'guess-style)
;;; guess-style.el ends here
