(require-package 'google-translate)

(setq-default google-translate-default-source-language "en"
              google-translate-default-target-language "zh-CN")

(setq google-translate-base-url
      "http://translate.google.cn/translate_a/single"
      google-translate-listen-url
      "http://translate.google.cn/translate_tts"
      google-translate--tkk-url
      "http://translate.google.cn/")

(setq google-translate-show-phonetic t)

(provide 'init-google-translate)
