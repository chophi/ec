(defconst org-beamer-prefix
  "\\documentclass[11pt]{beamer}
\\usepackage{orgformat}
\\usepackage{orgcolordef}
\\usepackage{graphicx}
\\usepackage{array}
\\usepackage{amsfonts}
\\usepackage{amsmath}
\\usepackage{subfigure}
\\usepackage[boxed,algosection]{algorithm2e}
\\usepackage{algorithmic}
\\usepackage{listings}
\\usepackage{tikz}
\\usepackage{animate}
\\renewcommand{\\figurename}{\\xiaowu\\kai{}图}
\\newcommand{\\mycaption}[1]{\\caption{\\xiaowu\\kai{}#1}}
\\setmainfont{Times New Roman}
\\setmonofont{Consolas}
\\setsansfont{Times New Roman}
\\usetikzlibrary{positioning}
\\tikzset{onslide/.code args={<#1>#2}{%
\\only<#1>{\\pgfkeysalso{#2}} % \\pgfkeysalso doesn't change the path
}}
\\tikzstyle{highlight}=[violet,ultra thick]
\\usetheme{Warsaw}
\\usecolortheme{orchid}"
)
(provide 'org-beamer-prefix)
