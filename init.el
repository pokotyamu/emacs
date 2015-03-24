; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ load-path

; load-pathの追加関数
(defun add-to-load-path (&rest paths)
    (let (path)
          (dolist (path paths paths)
                  (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
                            (add-to-list 'load-path default-directory)
                                    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
                                                    (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "yspel")


;; newline and indent
(global-set-key "\C-m" 'newline-and-indent)

;; Base settings
(setq read-file-name-completion-ignore-case t) ;; 補完で大文字小文字無視
(global-font-lock-mode t) ;;文字の色つけ
(display-time) ;;時計を表示
(auto-compression-mode t) ;;日本語infoの文字化け防止
(setq inhibit-startup-message t) ;;起動時のメッセージは消す
(setq-default tab-width 4 indent-tabs-mode nil);;tabは4文字分、改行後に自動インデント
(setq visible-bell t) ;; 警告音を消す
(show-paren-mode 1) ;; 対応する括弧を光らせる。
(global-hl-line-mode) ;; 編集行のハイライト
(setq require-final-newline t) ;; ファイル末の改行がなければ追加
;(menu-bar-mode -1) ;;メニューバーを消す
(tool-bar-mode 0) ;;ツールバーを表示しない
(setq truncate-partial-width-windows nil) ;; ウインドウ分割時に画面外へ出る文章を折り返す

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#800"
                    :height 0.5)


;; Window 分割を画面サイズに従って計算する
(defun split-window-vertically-n (num_wins)
    (interactive "p")
      (if (= num_wins 2)
                (split-window-vertically)
            (progn
                    (split-window-vertically
                            (- (window-height) (/ (window-height) num_wins)))
                          (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
    (interactive "p")
      (if (= num_wins 2)
                (split-window-horizontally)
            (progn
                    (split-window-horizontally
                            (- (window-width) (/ (window-width) num_wins)))
                          (split-window-horizontally-n (- num_wins 1)))))

;; Window 分割・移動を C-t で
(defun other-window-or-split ()
    (interactive)
      (when (one-window-p)
            (if (>= (window-body-width) 270)
                        (split-window-horizontally-n 3)
                    (split-window-horizontally)))
        (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;markdown記法の設定
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;ruby & railsの基本的な設定
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;;タブ幅の設定と、return後のインデント
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)
             (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)))
;;対応するdo-endのハイライト
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)
;;auto insert end 
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(defun ruby-electric-setup-keymap()
    (define-key ruby-mode-map " " 'ruby-electric-space)
    ;;;   (define-key ruby-mode-map "{" 'ruby-electric-curlies)
    ;;;   (define-key ruby-mode-map "(" 'ruby-electric-matching-char)
    ;;;   (define-key ruby-mode-map "[" 'ruby-electric-matching-char)
    ;;;   (define-key ruby-mode-map "\"" 'ruby-electric-matching-char)
    ;;;   (define-key ruby-mode-map "\'" 'ruby-electric-matching-char)
    ;;;   (define-key ruby-mode-map "|" 'ruby-electric-bar)
    )

;;erb-webmode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   4)
  (setq web-mode-css-offset    4)
  (setq web-mode-script-offset 4)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4)
  (setq indent-tabs-mode t)
  (setq tab-width 4))
(add-hook 'web-mode-hook 'web-mode-hook)
;; 色の設定
(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))
 )

                                        ;rabbit-mode
(when (locate-library "rabbit-mode")
  (autoload 'rabbit-mode "rabbit-mode" "major mode for Rabbit" nil t)
(add-to-list 'auto-mode-alist
             '("\\.\\(rbt\\|rab\\)$" . rabbit-mode)))

(when (locate-library "rd-mode")
(autoload 'rd-mode "rd-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.rd\\'" . rd-mode)))


;; run yatex mode when open .tex file
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; yatex load path
(setq load-path (cons (expand-file-name
                       "/Applications/Emacs.app/Contents/Resources/site-lisp/yatex")
                      load-path))
;; use utf-8 on yatex mode
(setq YaTeX-kanji-code 4)

;; run yspel
(require 'yspel)

;run with Japanese
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
    '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; run on specific mode
(add-hook 'yatex-mode-hook
          '(lambda () (flyspell-mode)))

;;use Preview.app
(setq dvi2-command "open -a Preview")
(defvar YaTeX-dvi2-command-ext-alist
    '(("xdvi" . ".dvi")
          ("ghostview\\|gv" . ".ps")
              ("acroread\\|pdf\\|Preview\\|open" . ".pdf")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-function-name-face ((t (:foreground "color-33")))))
