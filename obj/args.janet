(defn a/parse-args
  [args]
  (def the-args (array ;args))
  #
  (def head (get the-args 0))
  #
  (def default-opts @{:rest the-args})
  #
  (when (or (not head) (= head "-h") (= head "--help"))
    (break (merge {:help true} default-opts)))
  #
  (def opts
    (if-not (and (string/has-prefix? "{" head)
                 (string/has-suffix? "}" head))
      default-opts
      (let [parsed
            (try (parse (string "@" head))
              ([e] (eprint e)
                   (errorf "failed to parse options: %n" head)))]
        (assertf (and parsed (table? parsed))
                 "expected table but found: %s" (type parsed))
        (array/remove the-args 0)
        parsed)))
  #
  (when (nil? (get opts :pred))
    (put opts :pred identity))
  #
  (when (nil? (get opts :depth))
    (put opts :depth 1))
  #
  (merge opts default-opts
         # XXX: revisit
         (if (<= 2 (length the-args))
           {:paths-search true}
           {:enum-defs true})))

