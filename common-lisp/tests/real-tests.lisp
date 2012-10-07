
(in-package #:shovel-tests)

;;; "shovel-tests" goes here. Hacks and glory await!

(def-suite :shovel-tests)

(in-suite :shovel-tests)

(test test-constants
  (is (= 1 (shovel:naked-run-code "1")))
  (is (string= "a" (shovel:naked-run-code "'a'")))
  (is (= 1.23 (shovel:naked-run-code "1.23"))))

(test some-operators
  (is (= 2 (shovel:naked-run-code "1 + 1"))))

(defun run-tests ()
  (fiveam:run! :shovel-tests))
