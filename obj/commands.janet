(import ./find :prefix "")
(import ./report :prefix "")
(import ./search :prefix "")
(import ./utils :prefix "")

(defn c/search-and-report
  [opts]
  (def {:paths paths :query-fn query-fn :pattern pattern} opts)
  #
  (def [all-results _] (s/search-paths paths query-fn opts pattern))
  (when (zero? (length all-results))
    (break false))
  #
  (r/report all-results))

########################################################################

(defn c/all-defs
  [opts]
  (def {:depth depth :rest the-args} opts)
  #
  (def includes the-args)
  # find janetish files
  (def src-filepaths
    (s/collect-paths includes u/looks-like-janet?))
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
    (s/collect-paths includes u/looks-like-janet?))
  #
  (c/search-and-report {:query-fn f/find-def-of
                      :paths src-filepaths :pattern name
                      :depth depth}))

