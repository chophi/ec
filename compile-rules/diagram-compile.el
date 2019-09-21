(append
 ;; For graphviz-dot-mode
 (cu-make-mode-specific-custom-compile-rule-map
  'graphviz-dot-mode
  t
  '((?p . graphviz-dot-preview)
    (?v . graphviz-dot-view)
    (?e . graphviz-set-extension))
  '(("Output Image Extension" . graphviz-dot-preview-extension)))
 ;; For plantuml-mode
 (cu-make-mode-specific-custom-compile-rule-map
  'plantuml-mode
  t
  '((?p . plantuml-execute)
    (?v . plantuml-preview)
    (?e . graphviz-set-extension)
    (?t . toggle-plantuml-convert-to-latex))
  '(("Output Image Extension" . graphviz-dot-preview-extension)
    ("Convert to latex First" . plantuml-convert-to-latex)))
 ;; For latex-mode
 (cu-make-mode-specific-custom-compile-rule-map
  'latex-mode
  t
  '((?p . compile-tikz-to-svg)))

 ) ;; append

