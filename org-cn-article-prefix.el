(defconst cn-article-prefix
  "\\documentclass[a4paper,12pt]{article}
\\usepackage[top=2.5cm, bottom=2.2cm, left=2.8cm, right=2.8cm]{geometry}
\\usepackage{orgformat}
\\usepackage{orgcolordef}
\\usepackage{orgtitlesec}
\\usepackage{graphicx}   % 插入图片要用
\\usepackage{wrapfig}    % 图形环绕
\\usepackage{array}      % 表的详细设置
\\usepackage{hyperref}   % 链接设置
\\usepackage{subfigure}  % 插入多个表
\\usepackage{amsfonts}   % 一些特殊字体
\\usepackage{amsmath}
\\usepackage[boxed,algosection]{algorithm2e}
\\usepackage{algorithmic}
\\usepackage{listings}           % 插入代码
\\usepackage{paralist}           % 可将列表压缩
\\hypersetup{colorlinks=false,   % 设置链接颜色
pdfborder=0 0 0}              % 设置链接无 border
\\usepackage{xcolor}
\\usepackage{framed}
% \\usepackage[T1]{fontenc}
% \\usepackage{upquote}
\\newenvironment{command}
{\\colorlet{shadecolor}{darkgray} \\color{cyan} \\begin{snugshade*} \\Monaco\\xiaowu }
{\\end{snugshade*}}
\\renewcommand{\\contentsname}{\\sanhao\\hei{}目\\quad{}录}
\\renewcommand{\\figurename}{\\wuhao\\kai{}图}
\\renewcommand{\\tablename}{\\wuhao\\kai{}表}
\\renewcommand{\\algorithmcfname}{\\wuhao\\kai{}算法}
% 设置等式、表、图的标号为“节.序号”，每节序号重新计数
% numberwithin 在 amsmath 中
\\numberwithin{figure}{section}
\\numberwithin{table}{section}
\\numberwithin{equation}{section}
\\renewcommand{\\theequation}{\\wuhao\\kai \\arabic{section}.\\arabic{equation}}
\\renewcommand{\\thetable}{\\wuhao\\kai \\arabic{section}.\\arabic{table}}
\\renewcommand{\\thefigure}{\\wuhao\\kai \\arabic{section}.\\arabic{figure}}
% 使用subfig包的一些概念：
% \\thefigure是图的标号，
% \\thesubfigure是子图的标号，
% \\@thesubfigure设置子图\\caption的标号
% \\ref时子图显示的是\\p@subfigure\\thesubfigure
% 设置子图标标号为罗马，默认为\\alph{subfigure}
\\renewcommand{\\thesubfigure}{(\\roman{subfigure})}
\\makeatletter
\\renewcommand{\\@thesubfigure}{\\thesubfigure \\space}
\\renewcommand{\\p@subfigure}{\\thefigure :}
\\makeatother
% 设置各种引用
\\newcommand{\\reffig}[1]{图\\,\\ref{#1}}
\\newcommand{\\reftab}[1]{表\\,\\ref{#1}}
\\newcommand{\\refalg}[1]{算法\\,\\ref{#1}}
\\newcommand{\\upcite}[1]{\\textsuperscript{\\cite{#1}}} %参考文献应用使用右上角
\\setlength{\\lineskip}{20pt}
\\setlength{\\parskip}{0.5em}"
  )
(provide 'org-cn-article-prefix)
