(require-package 'tabbar)

(require 'tabbar)
;; 设置tabbar外观
;; 设置默认主题: 字体, 背景和前景颜色，大小
(set-face-attribute 'tabbar-default nil
		    :family "Inconsolata"
		    :height 120
		    )

;; 设置左边按钮外观：外框框边大小和颜色
(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    )
;; 设置当前tab外观：颜色，字体，外框大小和颜色
(set-face-attribute 'tabbar-selected nil
		    :inherit 'tabbar-default
		    :foreground "white"
		    :background "#8b3e2f"
		    :box '(:line-width 2 :style pressed-button)
		    :weight 'bold
		    )

;; 设置非当前tab外观：外框大小和颜色
(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "#eee9e9"
		    :foreground "#8b7d7b"
		    :box '(:line-width 2 :style released-button)
		    :slant 'normal
		    :weight 'ultra-light
		    )
(tabbar-mode)
(provide 'init-tabbar)
