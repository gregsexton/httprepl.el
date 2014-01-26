(require 's)
(require 'f)

(defvar httprepl-test-path
  (f-parent (f-this-file)))

(defvar httprepl-root-path
  (f-parent httprepl-test-path))

(require 'httprepl (f-expand "httprepl.el" httprepl-root-path))
