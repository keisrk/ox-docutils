(require 'test-simple)
(require 'org)
(require 'org-element)
;; M-x eval-buffer to execute the test.
(test-simple-start)

(assert-t (load-file "./ox-docutils.el")
          "Cannot find a file `ox-docutils.el'.")
(note "#1. Unit tests")
(assert-equal t (org-docutils-bibtex-citation-p (org-element-parse-secondary-string "\cite{Kroening2017}" (org-element-restriction 'latex-fragment))) "")
(end-tests)
