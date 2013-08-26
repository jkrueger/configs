(set-face-attribute 'default nil :font "Source Code Pro Semibold-10")

(load-theme 'noctilux t)

(require 'yasnippet)

;; Develop and keep personal snippets under ~/emacs.d/snippets
(setq yas/root-directory "~/.emacs.d/snippets/")

;; Load the snippets
;; (yas/initialize)
(yas/load-directory yas/root-directory)

