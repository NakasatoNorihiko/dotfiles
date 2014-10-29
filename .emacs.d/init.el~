;; ~/.emacs.d/elispディレクトリをロードパスに追加する
;; ただし、add-to-load-path関数を作成した場合は不要
;; (add-to-list 'load-path "~/.emacs.d/elisp/")
;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
;;Macだけに読み込ませる内容
(when (eq system-type 'darwin)
  ;;ファイル名を正しく扱うための設定
  (require 'ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
;;¥をバックスラッシュに変換
(define-key global-map [?¥] [?\\])
;;対応するカッコを表示する
(show-paren-mode 1)

;;行番号・桁番号を表示する
(global-linum-mode t)
(line-number-mode t)
(column-number-mode t)

;;現在行に色をつける
(global-hl-line-mode t)
(set-face-background 'hl-line "navy")

;;UTF-8にする
(prefer-coding-system 'utf-8)

;;背景を黒にする
(add-to-list 'default-frame-alist '(background-color . "black"))

;;文字色を設定する
(add-to-list 'default-frame-alist '(foreground-color . "white"))

;;起動時に画面を最大化する
(set-frame-parameter nil 'fullscreen 'maximized)

;; Mac用フォント設定
;; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/
;; 英語
 (set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 140)    ;; font size

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font
;; (font-spec :family "Hiragino Mincho Pro")) ;; font

;; 半角と全角の比を1:2にしたければ
(setq face-font-rescale-alist
;;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; Mac用フォント設定

;;フォントサイズを14にする
;;(add-to-list 'default-frame-alist '(font . "-unknown-Takaoゴシック-normal-normal-normal-*-14-*-*-*-d-0-iso10646-1"))

;;カーソル色を設定する 
(add-to-list 'default-frame-alist '(cursor-color . "white"))

;;選択中のリージョンの色を設定する
(set-face-background 'region "purple")

;;モードライン（アクティブでない）の文字色を設定します
(set-face-foreground 'mode-line-inactive "gray30")

;;モードライン（アクティブでない）の背景色を設定します
(set-face-background 'mode-line-inactive "gray70")

;;
(global-font-lock-mode t)

;最終行に必ず一行追加する
(setq require-final-newline t)

;バッファの終端での新規行追加禁止
(setq next-line-add-newlines nil)

;関数名を表示
(which-function-mode t)

;スペルチェック実行
(setq-default flyspell-mode t)
(setq ispell-dictionary "american")

;起動時の画面はいらない
(setq inhibit-startup-message t)

;スクロールバーもツールバーもいらない
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;time-stampをつける
(require 'time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "//Time-stamp:[ ]+<")
(setq time-stamp-end ">")

;;引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "conf" "public_repos")

;;Auto Complete Modeの設定
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)
(add-hook 'auto-complete-mode-hook
	  (lambda ()
	    (define-key ac-completing-map "\r" 'ac-stop) ;;returnで自動補完停止
	    (define-key ac-completing-map " " 'ac-complete) ;;スペースで確定
	    ))

;;Cの全自動インデント設定
(add-hook 'c-mode-hook 
	  '(lambda ()
	     (c-set-style "k&r") ;;k&rスタイル
	     (setq c-auto-newline t) ;自動インデントを有効にする
	     (setq tab-width 4) ;;タブ幅４
	     (setq c-basic-offset 4) ;;基本タブ幅４
	     (setq indent-tabs-mode nil) ;インデントは空白文字で行う
	     (c-set-offset 'arglist-close 0) ;関数の引数リストの閉じ括弧はインデントしない
	     ))

;;C++の全自動インデント設定
(add-hook 'c++-mode-hook 
	  '(lambda ()
	     (c-set-style "stroustrup") ;;stroustrupスタイル
	     (setq c-auto-newline t) ;自動インデントを有効にする
	     (setq tab-width 4) ;;タブ幅４
	     (setq c-basic-offset 4) ;;基本タブ幅４
	     (setq indent-tabs-mode nil) ;インデントは空白文字で行う
	     (c-set-offset 'arglist-close 0) ;関数の引数リストの閉じ括弧はインデントしない
	     ))

;Elisp編集時の設定
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil))) ;;インデントは空白で行う
;
