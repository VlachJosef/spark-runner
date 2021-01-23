;;; spark-comint.el --- Utility for running Spark jobs locally -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Josef Vlach

(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'sbt-mode-project)

(defvar-local spark-package nil)
(defvar-local spark-main-class nil)

(defvar spark-submit-executable "spark-submit")

(defvar spark-comint--mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "C-c r") 'spark-package)
    map)
  "Basic mode map for `spark-comint-mode'.")

(defun spark-comint--sentinel (process event)
  "Display PROCESS buffer if spark-submit exited with abnormal EVENT."
  (when (string-match "exited abnormally" event)
    (pop-to-buffer (process-buffer process))))

(defun spark-comint--get-buffer-name ()
  "Return buffer name for spark-submit's stdout/stderr output."
  (format "*spark-submit %s*" (sbt:find-root)))

(defun spark-comint--run (package main-class arguments)
  "Run spark job in comint mode.

ARGUMENTS are command line arguments for spark executable.
When run it will kill existing process if one exists."
  (let ((buffer-name (spark-comint--get-buffer-name))
        (inhibit-read-only t))

    (when (not (executable-find spark-submit-executable))
      (error "Error: Could not find %s on PATH.  Please customize the spark-submit-executable variable" spark-submit-executable))

    (with-current-buffer (get-buffer-create buffer-name)
      (unless (derived-mode-p 'spark-comint-mode)
        (spark-comint-mode)
        (buffer-disable-undo)
        (setq spark-package package)
        (setq spark-main-class main-class))
      (erase-buffer)
      (let ((buffer (comint-exec (current-buffer) buffer-name spark-submit-executable nil arguments)))
        (set-process-sentinel (get-buffer-process buffer) 'spark-comint--sentinel)
        (switch-to-buffer buffer)
        buffer))))

(define-derived-mode spark-comint-mode comint-mode "spark-comint"
  "Major mode for spark-comint.

\\{spark-comint--mode-map}"
  (use-local-map spark-comint--mode-map)
  (add-hook 'spark-comint-mode-hook 'spark-comint--initialize-for-comint-mode)
  (add-hook 'spark-comint-mode-hook 'spark-comint--initialize-for-compilation-mode))

(defun spark-comint--initialize-for-comint-mode ()
  "Initialize buffer for comint mode support."
  (when (derived-mode-p 'comint-mode)
    (setq comint-process-echoes nil)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))
    (setq ansi-color-for-comint-mode t)))

(defun spark-comint--initialize-for-compilation-mode ()
  "Initialize buffer for compilation mode support."
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-setup t))

(provide 'spark-comint)
;;; spark-comint.el ends here
