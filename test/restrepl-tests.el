(require 's)

(ert-deftest restrepl-read-simple ()
  (should (equal "GET"
                 (cdr (assoc 'method (restrepl-read "GET http://www.gregsexton.org")))))
  (should (equal "http://www.gregsexton.org"
                 (cdr (assoc 'url (restrepl-read "GET http://www.gregsexton.org"))))))

(ert-deftest read-horrible-uri ()
  (let ((any-uri "fdjk  %2sfj 6sj^sjv\`,mc/f  jkfda 1234 {} "))
    (should (equal any-uri (cdr (assoc 'url (restrepl-read (s-concat "GET " any-uri))))))))
