(import ./find :as f)
(import ./itemize :as i)
(import ./report :as r)
(import ./search :as s)
(import ./utils :as u)

(defn search-and-report
  [opts]
  (def {:paths paths :query-fn query-fn :pattern pattern} opts)
  #
  (def [all-results _] (s/search-paths paths query-fn opts pattern))
  (when (zero? (length all-results))
    (break false))
  #
  (r/report all-results))

########################################################################

(defn all-defs
  [opts]
  (def {:depth depth :rest the-args} opts)
  #
  (def includes the-args)
  # find janetish files
  (def src-filepaths
    (filter |(and (= :file (os/stat $ :mode))
                  (u/looks-like-janet? $))
            (i/itemize ;includes)))
  #
  (search-and-report {:query-fn f/find-defs
                      :paths src-filepaths
                      :depth depth}))

(defn def-of
  [opts]
  (def {:depth depth :rest the-args} opts)
  #
  (def name (get the-args 0))
  (array/remove the-args 0)
  #
  (def includes the-args)
  # find janetish files
  (def src-filepaths
    (filter |(and (= :file (os/stat $ :mode))
                  (u/looks-like-janet? $))
            (i/itemize ;includes)))
  #
  (search-and-report {:query-fn f/find-def-of
                      :paths src-filepaths :pattern name
                      :depth depth}))

