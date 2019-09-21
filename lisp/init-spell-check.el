;; 1. brew install hunspell
;; 2. Copy the en_US.aff and en_US.dic to ~/Library/Setting
(when (equal os 'macos)
  (let ((possible-spell-program (cu-search-brew-executable "hunspell")))
    (when possible-spell-program
      (setenv "DICTIONARY" "en_US")
      (setq ispell-program-name possible-spell-program
            ispell-dictionary "en_US")
      (defconst my-ispell-is-enabled t "Whether ispell is enabled"))))

(when (and (equal os 'linux) (executable-find "hunspell"))
  (setenv "DICTIONARY" "en_US")
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US")
  (defconst my-ispell-is-enabled t "Whether ispell is enabled"))

(provide 'init-spell-check)
