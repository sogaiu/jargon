(import ./search :prefix "")
(import ./find :prefix "")

(def c/janet-indent
  "Default indentation that the built-in `doc-format` uses."
  4)

(defn c/search-and-report
  [opts]
  (def {:query-fn query-fn
        :name name} opts)
  #
  (def [all-results _] (s/search-paths query-fn opts))
  (when (zero? (length all-results))
    (break false))
  #
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
                  c/janet-indent))
    (printf (string/repeat "#" width))
    (print)
    (++ i))
  #
  all-results)

########################################################################

(defn c/all-defs
  [opts]
  (def {:depth depth :rest the-args} opts)
  #
  (def includes the-args)
  # find janetish files
  (def src-filepaths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  #
  (c/search-and-report {:query-fn f/find-defs
                      :paths src-filepaths
                      :depth depth}))

(defn c/def-of
  [opts]
  (def {:depth depth :rest the-args} opts)
  #
  (def name (get the-args 0))
  (array/remove the-args 0)
  #
  (def includes the-args)
  # find janetish files
  (def src-filepaths
    (s/collect-paths includes |(or (string/has-suffix? ".janet" $)
                                   (s/has-janet-shebang? $))))
  #
  (c/search-and-report {:query-fn f/find-def-of
                      :paths src-filepaths :name name
                      :depth depth}))

