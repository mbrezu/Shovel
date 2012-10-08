
(in-package #:shovel-tests)

;;; "shovel-tests" goes here. Hacks and glory await!

(def-suite :shovel-tests)

(in-suite :shovel-tests)

(test test-constants
  (is (= 1 (shovel:naked-run-code (list "1"))))
  (is (string= "a" (shovel:naked-run-code (list "'a'"))))
  (is (= 1.23 (shovel:naked-run-code (list "1.23")))))

(test some-operators
  (is (= 2 (shovel:naked-run-code (list "1 + 1")))))

(test complex
  (is (equalp #(1 2 3 3 4 5)
              (shovel:naked-run-code
               (list (shovel:stdlib)
                     "stdlib.sort(array(3, 1, 2, 5, 4, 3),
                                  fn (a, b) a < b,
                                  fn (a, b) a == b)")))))

(defun run-tests ()
  (fiveam:run! :shovel-tests))
