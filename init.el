
;;railsの基本的な設定
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))


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
