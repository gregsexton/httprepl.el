(require 's)

(ert-deftest restrepl-read-simple ()
  (should (equal "GET"
                 (cdr (assoc 'method (restrepl-read "GET http://www.gregsexton.org")))))
  (should (equal "http://www.gregsexton.org"
                 (cdr (assoc 'url (restrepl-read "GET http://www.gregsexton.org"))))))

(ert-deftest read-horrible-uri ()
  (let ((any-uri "fdjk  %2sfj 6sj^sjv\`,mc/f  jkfd:a 1234 {} "))
    (should (equal any-uri (cdr (assoc 'url (restrepl-read (s-concat "GET " any-uri))))))))

(ert-deftest read-uri-with-method ()
  (should (equal "POST"
                 (cdr (assoc 'method (restrepl-read "POST http://www.gregsexton.org/POST")))))
  (should (equal "http://www.gregsexton.org/POST"
                 (cdr (assoc 'url (restrepl-read "POST http://www.gregsexton.org/POST"))))))

(ert-deftest read-header ()
  (dolist (input '("GET url\nkey : value"
                   "GET url\nkey: value"
                   "GET url\nkey :value"
                   "GET url\nkey:value"))
    (should (equal "value"
                   (->> input
                     restrepl-read
                     (assoc 'headers)
                     cdr
                     (assoc "key")
                     cdr)))))

(ert-deftest read-no-headers ()
  (should (equal nil
                 (cdr (assoc 'headers (restrepl-read "GET http://www.gregsexton.org"))))))

(ert-deftest read-headers ()
  (should (equal '(("key2" . "value2") ("key1" . "value1"))
                 (->> "GET url\nkey1: value1\nkey2 : value2"
                   restrepl-read
                   (assoc 'headers)
                   cdr))))

(ert-deftest read-entity-without-headers ()
  (should (equal "some text\nafter everything"
                 (->> "GET url\n\nsome text\nafter everything"
                   restrepl-read
                   (assoc 'entity)
                   cdr))))

(ert-deftest read-entity-with-headers ()
  (should (equal "some text\n\n  GET  after everything"
                 (->> "GET url\nkey1: value1\nkey2 : value2\n\nsome text\n\n  GET  after everything"
                   restrepl-read
                   (assoc 'entity)
                   cdr))))
