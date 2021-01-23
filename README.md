# Utility for running Spark jobs locally

## How to install

```elisp
(use-package spark-runner
  :config
  (setq spark-submit-executable "~/spark/spark-3.0.1/bin/spark-submit")
  :after (scala-mode sbt-mode)
  :bind (:map scala-mode-map
         ("C-c r" . spark-package)
         :map sbt:mode-map
         ("C-c r" . spark-package)))
```
