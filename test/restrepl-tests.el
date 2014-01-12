(require 's)

(ert-deftest restrepl-read-simple ()
  (should (equal 'get
                 (cdr (assoc 'method (restrepl-read "GET http://www.gregsexton.org\n")))))
  (should (equal "http://www.gregsexton.org"
                 (cdr (assoc 'url (restrepl-read "GET http://www.gregsexton.org\n"))))))

(ert-deftest read-horrible-uri ()
  (let ((any-uri "fdjk  %2sfj 6sj^sjv\`,mc/f  jkfda 1234 {} "))
    (should (equal any-uri (cdr (assoc 'url (restrepl-read (s-concat "GET " any-uri "\n"))))))))
