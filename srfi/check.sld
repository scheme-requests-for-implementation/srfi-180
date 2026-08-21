; SPDX-FileCopyrightText: 2026 Amirouche Boubekki
;
; SPDX-License-Identifier: MIT

;; test framework
(define-library (check)
  (export check check-raise skip pk failure? failure-expected failure-actual)

  (import (scheme base))
  (import (scheme write))

  (include "check.scm"))
