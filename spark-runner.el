;;; spark-runner.el --- Utility for running Spark jobs locally -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Josef Vlach

(require 'sbt-mode)
(require 'spark-comint)

(defconst spark-package-regexp "^\\[info][[:space:]]+\\(/.*\\)")
(defconst spark-main-class-regexp "^\\[info\\][[:space:]]+Some(\\(.*\\))$")

(defun spark-package ()
  (interactive)
  (unless (eq major-mode 'sbt-mode)
    (sbt-switch-to-active-sbt-buffer))

  (if (get-buffer (spark-comint--get-buffer-name))
      (spark-package-only)
    (spark-run-package-in-local)))

(defun spark-rerun-current ()
  (when (get-buffer (spark-comint--get-buffer-name))
    (with-current-buffer (spark-comint--get-buffer-name)
      (switch-to-buffer-other-window (current-buffer))
      (let ((artifact-path spark-package)
            (main-class spark-main-class))
        (spark-comint--run artifact-path main-class `("--class" ,main-class "--master" "local[4]" ,artifact-path))))))

(defun sbt-hydra:get-main-class (sbt-output)
  (let ((artifact-path (car (sbt-hydra:match-regex-in-sbt-output sbt-output spark-package-regexp)))
        (main-class (car (sbt-hydra:match-regex-in-sbt-output sbt-output spark-main-class-regexp))))
    (message "artifact-path: %s, main-class %s" artifact-path main-class)
    (spark-comint--run artifact-path main-class `("--class" ,main-class "--master" "local[4]" ,artifact-path))))

(defun sbt-hydra:on-package-done (sbt-output)
  (spark-rerun-current))

(defun spark-parse-package-name (sbt-output)
  (sbt-hydra:parse-sbt-output sbt-output 'sbt-hydra:get-main-class 'spark-parse-package-name))

(defun spark-parse-package (sbt-output)
  (sbt-hydra:parse-sbt-output sbt-output 'sbt-hydra:on-package-done  'spark-parse-package))

(defun spark-run-package-in-local ()
  (add-hook 'comint-output-filter-functions 'spark-parse-package-name)
  (setq sbt-hydra:sbt-output-cleared "")
  (sbt:command "show package mainClass"))

(defun spark-package-only ()
  (add-hook 'comint-output-filter-functions 'spark-parse-package)
  (setq sbt-hydra:sbt-output-cleared "")
  (sbt:command "package"))

(provide 'spark-runner)
;;; spark-runner.el ends here
