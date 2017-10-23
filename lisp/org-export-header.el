(defconst cn-article-header
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
  "the headline for cn-article latex export")

(defconst org-slides-header
  "\\documentclass[11pt]{slides}
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
  "the headline for slides latex export")

;; use the tomorrow-eighties style
(setq org-html-head
  "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: sans-serif; font-size: 12pt;}
  .title  { text-align: center; color: #f99157}
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  p, ul, ol {-webkit-margin-before: 0em; -webkit-margin-after: 0em;}
  div[id^=\"text-table-of-contents\"] {color: #99ffff}
  [href] {color: #ff7f00;}
  h3[id^=\"sec\"] {color: #9aff9a}
  h4[id^=\"sec\"] {color: #9aff9a}
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #a9a9a9;
	padding: 5pt;
	font-family: consolas, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>")

(provide 'org-export-header)
