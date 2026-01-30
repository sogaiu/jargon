(import ./jipper :as j)

# XXX: if the keys were strings then other code would need to
#      change...
(def func-definers
  {'defn 1 'defn- 1})

(def macro-definers
  {'defmacro 1 'defmacro- 1})

(def call-definers
  (merge func-definers macro-definers))

(def special-definers
  {'def 1 'def- 1
   'var 1 'var- 1})

# XXX: no defglobal or varglobal
(def definers
  (merge call-definers special-definers {'defdyn 1}))

(defn find-def-of
  [src opts]
  (def {:pattern name} opts)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $)
                             [:symbol _ sym]
                             (when (string/has-suffix? name sym)
                               $))))
    (def parent-zloc (j/up next-zloc))
    (def node (j/node parent-zloc))
    (def [node-type node-loc _] node)
    (when (= :tuple node-type)
      (def raw-code-str (j/gen node))
      (def parsed
        (try
          (parse raw-code-str)
          ([e]
            (eprintf "failed to parse: %s" raw-code-str))))
      (when parsed
        (def found-name (string (get parsed 1)))
        (when (string/has-suffix? name found-name)
          (def head (first parsed))
          (cond
            (get definers head)
            (let [leading-ws (string/repeat " " (dec (get node-loc :bc)))
                  src (string leading-ws (j/gen node))]
              (array/push results
                          @{:loc node-loc
                            :def-type (string head)
                            :found-name found-name
                            :src src}))
            # XXX: other cases?
            nil))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (find-def-of
    ``
    (defn smile
      "I am a defn docstring."
      [y]
      (pp y))

    (defn- smile
      "I am a defn- docstring."
      [z]
      (pp [:z z]))
    ``
    {:pattern "smile"})
  # =>
  @[@{:def-type "defn"
      :found-name "smile"
      :loc @{:bc 1 :bl 1 :ec 10 :el 4}
      :src
      (string
        "(defn smile\n"
        "  \"I am a defn docstring.\"\n"
        "  [y]\n"
        "  (pp y))")}
    @{:def-type "defn-"
      :found-name "smile"
      :loc @{:bc 1 :bl 6 :ec 15 :el 9}
      :src
      (string
        "(defn- smile\n"
        "  \"I am a defn- docstring.\"\n"
        "  [z]\n"
        "  (pp [:z z]))")}]

  (find-def-of
    ``
    (var smile "a docstring" {:a 2})

    (var- smile "woohoo" "hello")
    ``
    {:pattern "smile"})
  # =>
  @[@{:def-type "var"
      :found-name "smile"
      :loc @{:bc 1 :bl 1 :ec 33 :el 1}
      :src `(var smile "a docstring" {:a 2})`}
    @{:def-type "var-"
      :found-name "smile"
      :loc @{:bc 1 :bl 3 :ec 30 :el 3}
      :src `(var- smile "woohoo" "hello")`}]

  (find-def-of
    ``
    (defdyn *smile*)

    (defdyn *smile* "smiling docstring")
    ``
    {:pattern "*smile*"})
  # =>
  @[@{:def-type "defdyn"
      :found-name "*smile*"
      :loc @{:bc 1 :bl 1 :ec 17 :el 1}
      :src "(defdyn *smile*)"} 
    @{:def-type "defdyn"
      :found-name "*smile*"
      :loc @{:bc 1 :bl 3 :ec 37 :el 3}
      :src `(defdyn *smile* "smiling docstring")`}]

  (find-def-of
    ```
    (defmacro as-macro
      ``Use a function or macro literal `f` as a macro. This lets
      any function be used as a macro. Inside a quasiquote, the
      idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used
      to avoid unwanted variable capture of `my-custom-macro`.``
      [f & args]
      (f ;args))
    ```
    {:pattern "as-macro"})
  # =>
  @[@{:def-type "defmacro"
      :found-name "as-macro"
      :loc @{:bc 1 :bl 1 :ec 13 :el 7}
      :src
      (string 
        "(defmacro as-macro\n"
        "  ``Use a function or macro literal `f` as a macro. This lets\n"
        "  any function be used as a macro. Inside a quasiquote, the\n"
        "  idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used\n"
        "  to avoid unwanted variable capture of `my-custom-macro`.``\n"
        "  [f & args]\n"
        "  (f ;args))")}]

  (find-def-of
    ``
    (def smile "a docstring" 1)

    (defn smile
      "I am a docstring."
      [y]
      (pp y))
    ``
    {:pattern "smile"})
  # =>
  @[@{:def-type "def"
      :found-name "smile"
      :loc @{:bc 1 :bl 1 :ec 28 :el 1}
      :src `(def smile "a docstring" 1)`}
    @{:def-type "defn"
      :found-name "smile"
      :loc @{ :bc 1 :bl 3 :ec 10 :el 6}
      :src "(defn smile\n  \"I am a docstring.\"\n  [y]\n  (pp y))"}]

  )

(defn find-defs
  [src &opt opts]
  (default opts {})
  (def {:depth depth :pred pred} opts)
  (default depth 1)
  #
  (def tree (j/par src))
  (var cur-zloc (j/zip-down tree))
  (def results @[])
  #
  (while (def next-zloc
           (j/search-from cur-zloc
                          |(match (j/node $)
                             [:tuple]
                             $)))
    (when (<= (length (j/path next-zloc))
              depth)
      (def node (j/node next-zloc))
      (def [node-type node-loc _] node)
      (def raw-code-str (j/gen node))
      (def parsed
        (try
          (parse raw-code-str)
          ([e]
            (eprintf "failed to parse: %s" raw-code-str))))
      (when (and parsed
                 (if-not pred true (pred parsed)))
        (when-let [head (first parsed)]
          (when (symbol? head)
            (def name (string (get parsed 1)))
            (cond
              (get definers head)
              (let [leading-ws (string/repeat " " (dec (get node-loc :bc)))
                    src (string leading-ws (j/gen node))]
                (array/push results
                            @{:loc node-loc
                              :def-type (string head)
                              :found-name name
                              :src src}))
              # XXX: other cases?
              nil)))))
    #
    (set cur-zloc (j/df-next next-zloc)))
  #
  results)

(comment

  (find-defs
    ``
    (defn smile
      "I am a defn docstring."
      [y]
      (pp y))

    (defn- smile
      "I am a defn- docstring."
      [z]
      (pp [:z z]))
    ``)
  # =>
  @[@{:def-type "defn"
      :found-name "smile"
      :loc @{:bc 1 :bl 1 :ec 10 :el 4}
      :src
      (string "(defn smile\n"
              "  \"I am a defn docstring.\"\n"
              "  [y]\n"
              "  (pp y))")}
    @{:def-type "defn-"
      :found-name "smile"
      :loc @{:bc 1 :bl 6 :ec 15 :el 9}
      :src
      (string "(defn- smile\n"
              "  \"I am a defn- docstring.\"\n"
              "  [z]\n"
              "  (pp [:z z]))")}]

  (find-defs
    ``
    (var smile "a docstring" {:a 2})

    (var- smile "woohoo" "hello")
    ``)
  # =>
  @[@{:def-type "var" 
      :found-name "smile" 
      :loc @{ :bc 1 :bl 1 :ec 33 :el 1} 
      :src `(var smile "a docstring" {:a 2})`} 
    @{:def-type "var-" 
      :found-name "smile" 
      :loc @{ :bc 1 :bl 3 :ec 30 :el 3} 
      :src `(var- smile "woohoo" "hello")`}]

  (find-defs
    ``
    (defdyn *smile*)

    (defdyn *smile* "smiling docstring")
    ``)
  # =>
  @[@{:def-type "defdyn" 
      :found-name "*smile*" 
      :loc @{ :bc 1 :bl 1 :ec 17 :el 1} 
      :src "(defdyn *smile*)"} 
    @{:def-type "defdyn" 
      :found-name "*smile*" 
      :loc @{ :bc 1 :bl 3 :ec 37 :el 3} 
      :src `(defdyn *smile* "smiling docstring")`}]

  (find-defs
    ```
    (defmacro as-macro
      ``Use a function or macro literal `f` as a macro. This lets
      any function be used as a macro. Inside a quasiquote, the
      idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used
      to avoid unwanted variable capture of `my-custom-macro`.``
      [f & args]
      (f ;args))
    ```)
  # =>
  @[@{:def-type "defmacro" 
      :found-name "as-macro" 
      :loc @{ :bc 1 :bl 1 :ec 13 :el 7} 
      :src
      (string
        "(defmacro as-macro\n"
        "  ``Use a function or macro literal `f` as a macro. This lets\n"
        "  any function be used as a macro. Inside a quasiquote, the\n"
        "  idiom `(as-macro ,my-custom-macro arg1 arg2...)` can be used\n"
        "  to avoid unwanted variable capture of `my-custom-macro`.``\n"
        "  [f & args]\n  (f ;args))")}]

  (find-defs
    ``
    (def smile "a docstring" 1)

    (defn smile
      "I am a docstring."
      [y]
      (pp y))
    ``)
  # =>
  @[@{:def-type "def"
      :found-name "smile"
      :loc @{:bc 1 :bl 1 :ec 28 :el 1}
      :src `(def smile "a docstring" 1)`}
    @{:def-type "defn"
      :found-name "smile"
      :loc @{:bc 1 :bl 3 :ec 10 :el 6}
      :src "(defn smile\n  \"I am a docstring.\"\n  [y]\n  (pp y))"}]

  )

