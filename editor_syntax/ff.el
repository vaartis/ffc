
(setq ff-keywords '("fnc" "extern" "operator"
                    "include" "type" "ref" "val" "implement" "for"
                    "destructor" "if" "while" "else" "ret"))
(setq ff-consts '("true" "false"))
(setq ff-types '("int" "float" "bool" "str"))

(setq ff-keywords-regexp (regexp-opt ff-keywords 'words))
(setq ff-consts-regexp (regexp-opt ff-consts 'words))
(setq ff-types-regexp (regexp-opt ff-types 'words))

(setq ff-font-lock-keywords
      `(
        (,ff-keywords-regexp . font-lock-keyword-face)
        (,ff-consts-regexp . font-lock-constant-face)
        ("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\) ?(" 1 'font-lock-function-name-face)
        (,ff-types-regexp . font-lock-type-face)))

;;;###autoload
(define-derived-mode ff-mode c-mode
  "F.F. mode"
  (setq font-lock-defaults '((ff-font-lock-keywords))))

(add-to-list 'auto-mode-alist '("\\.ff\\'" . ff-mode))

(setq ff-keywords nil)
(setq ff-consts nil)
(setq ff-types nil)

(setq ff-keywords-regexp nil)
(setq ff-consts-regexp nil)
(setq ff-types-regexp nil)

(provide 'ff-mode)
