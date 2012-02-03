(test-iso "split-by works"
  '("abc" "d")
  (split-by "#" "abc#d"))

(test-iso "strip-after works"
  "abc"
  (strip-after "abc#d" "#"))

(test-iso "strip-after works when delimiter is absent"
  "abc"
  (strip-after "abc" "#"))

(test-iso "parse-url works"
  (obj resource "http" host "example.com" port 80 filename "foo")
  (parse-url "http://example.com/foo"))

(test-iso "parse-url detects port"
  (obj resource "http" host "example.com" port 81 filename "foo")
  (parse-url "http://example.com:81/foo"))

(test-iso "parse-url detects nested directories"
  (obj resource "http" host "example.com" port 81 filename "foo/bar")
  (parse-url "http://example.com:81/foo/bar"))

(test-iso "parse-url gives https the right port"
  (obj resource "https" host "example.com" port 443 filename "foo/bar")
  (parse-url "https://example.com/foo/bar"))

(test-iso "parse-url makes url http by default"
  (obj resource "http" host "example.com" port 81 filename "foo/bar")
  (parse-url "example.com:81/foo/bar"))

(test-iso "parse-url ignores anchor"
  (obj resource "http" host "example.com" port 80 filename "foo")
  (parse-url "http://example.com/foo#anchor"))

(test-iso "parse-url handles query string"
  (obj resource "http" host "example.com" port 80 filename "foo" query "bar=blah")
  (parse-url "http://example.com/foo?bar=blah#anchor"))
