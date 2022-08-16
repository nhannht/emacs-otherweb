(require 'ert)
(require 'otherweb)
(ert-deftest otherweb--create-endpoint-test ()
  "Test otherweb--create-endpoint "
  (should (string=  (url-type  (url-generic-parse-url (otherweb--create-endpoint))) "https"))
  (should (stringp (otherweb--create-endpoint)))

  )

(ert-deftest otherweb--getdata-test ()
  "Test otherweb--getdata "
  (let (( data (otherweb--getdata)))
    (should (string= (caar data) "items")
            )
    (should (length> (cdar data) 20)) )
  )
