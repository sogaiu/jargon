(def janet-indent
  "Default indentation that the built-in `doc-format` uses."
  4)

(defn report
  [all-results]
  (print)
  (var i 1)
  (each r all-results
    (def {:path path :loc {:bc col-no :bl line-no}
          :def-type def-type :src src} r)
    (printf "# %d # %s on line %d, column %d" i path line-no col-no)
    (print)
    (print src)
    (print)
    # XXX: 72 = 80 - 8 (from janet's `boot.janet`)
    (def width (- (- (dyn :doc-width 80) 8)
                  janet-indent))
    (printf (string/repeat "#" width))
    (print)
    (++ i))
  #
  all-results)

