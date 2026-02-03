(import ./args :prefix "")
(import ./commands :prefix "")

(def version "DEVEL")

(def usage
  ``
  Usage: jagn <patt> <file-or-dir>...
         jagn <file-or-dir>
         jagn [-h|--help]

  Find and display janet definitons.

  Parameters:

    <patt>                 string to query with
    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output

  Examples:

    Show definition(s) of `zipper` in `src/jipper.janet`:

    $ jagn zipper src/jipper.janet

    Show definitions in files under `data/`:

    $ jagn data
  ``)

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (def {:rest the-args} opts)
  #
  (def arg (get the-args 0))
  #
  (cond
    (get opts :help)
    (print usage)
    #
    (get opts :enum-defs)
    (let [results (c/all-defs opts)]
      (when (and (not results) (not (get opts :dump)))
        (print "Nothing found")))
    # base results on files and/or directories searching
    (get opts :paths-search)
    (let [results (c/def-of opts)]
      (when (and (not results) (not (get opts :dump)))
        (print "Nothing found")))
    # XXX: don't expect to get here
    (errorf "bug somewhere: args: %n opts: %n" args opts)))

