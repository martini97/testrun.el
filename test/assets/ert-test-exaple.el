(ert-deftest test-testrun-pytest-get-test-nearest ()
  "Verify expected test paths with the nearest scope.

The nearest scope should return the full path to the current test function.
If it's outside of a function, but inside a test class, then it should return
the path to the test class.
If it's outside of a class it should return the path to the file."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode python-ts-mode
        :language python
        :asset "test_python_pytest.py"
        :position 72
        :body
        (progn
          (should (equal (testrun-pytest-get-test "nearest")
                         "test/assets/test_python_pytest.py::test_root_level"))
          (goto-char 126)
          (should (equal (testrun-pytest-get-test "nearest")
                         "test/assets/test_python_pytest.py::TestWithNamespace"))
          (goto-char 205)
          (should (equal (testrun-pytest-get-test "nearest")
                         "test/assets/test_python_pytest.py::TestWithNamespace::test_inside_namespace"))))))

(ert-deftest test-testrun-pytest-get-test-namespace ()
  "Verify expected test paths with the namespace scope.

The nearest scope should return the full path to the current test namespace.
If it's outside of a class it should return the path to the file."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode python-ts-mode
        :language python
        :asset "test_python_pytest.py"
        :position 72
        :body
        (progn
          (should (equal (testrun-pytest-get-test "namespace")
                         "test/assets/test_python_pytest.py"))
          (goto-char 126)
          (should (equal (testrun-pytest-get-test "namespace")
                         "test/assets/test_python_pytest.py::TestWithNamespace"))
          (goto-char 205)
          (should (equal (testrun-pytest-get-test "namespace")
                         "test/assets/test_python_pytest.py::TestWithNamespace"))))))

(ert-deftest test-testrun-pytest-get-test-file ()
  "Verify expected test paths with the file scope.

Should always return the path to the current file."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode python-ts-mode
        :language python
        :asset "test_python_pytest.py"
        :position 72
        :body
        (progn
          (should (equal (testrun-pytest-get-test "file")
                         "test/assets/test_python_pytest.py"))
          (goto-char 126)
          (should (equal (testrun-pytest-get-test "file")
                         "test/assets/test_python_pytest.py"))
          (goto-char 205)
          (should (equal (testrun-pytest-get-test "file")
                         "test/assets/test_python_pytest.py"))))))

(ert-deftest test-testrun-pytest-get-test-all ()
  "Verify expected test paths with the all scope.

Should always return the empty."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory))))
      (test-testrun-treesit-setup
        :mode python-ts-mode
        :language python
        :asset "test_python_pytest.py"
        :position 72
        :body
        (progn
          (should (equal (testrun-pytest-get-test "all") ""))
          (goto-char 126)
          (should (equal (testrun-pytest-get-test "all") ""))
          (goto-char 205)
          (should (equal (testrun-pytest-get-test "all") ""))))))

(ert-deftest test-testrun-pytest-run ()
  "Sort of an integration test for the compile command of pytest."
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should (equal cmd "pytest test/assets/test_python_pytest.py::test_root_level"))
               (should commint))))
    (test-testrun-treesit-setup
     :mode python-ts-mode
     :language python
     :asset "test_python_pytest.py"
     :position 72
     :body
     (testrun-nearest)))
  ;; running pytest from virtualenv, with flags
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should (equal cmd ".venv/bin/pytest --disable-warnings test/assets/test_python_pytest.py::test_root_level"))
               (should commint))))
    (let ((testrun-runners '((pytest . (".venv/bin/pytest" "--disable-warnings")))))
      (test-testrun-treesit-setup
       :mode python-ts-mode
       :language python
       :asset "test_python_pytest.py"
       :position 72
       :body
       (testrun-nearest))))
  ;; running with commint disabled
  (cl-letf (((symbol-function 'project-current)
             (lambda () (list 'vc 'Git default-directory)))
            ((symbol-function 'compile)
             (lambda (cmd commint)
               (should (equal cmd "pytest test/assets/test_python_pytest.py::test_root_level"))
               (should-not commint))))
    (let ((testrun-comint-runners nil))
      (test-testrun-treesit-setup
       :mode python-ts-mode
       :language python
       :asset "test_python_pytest.py"
       :position 72
       :body
       (testrun-nearest)))))
