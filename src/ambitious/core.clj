;; Compiler for m-MicroLang
;; Use 'lein test' for evaluation

(ns ambitious.core
  (:gen-class))

(defn generate-code
  "Generate Clojure code (as data) from m-MicroLang (list of symbols)"
  [statements]
  (let [generated-code (atom '(do))
        append-code (fn [c] (swap! generated-code concat (list c)))]
    (doall (map-indexed (fn [i statements-group]
      (let [statement (str statements-group)]
        (cond
          (re-matches #"<nil>" statement)
            (append-code `(swap! ~'stack concat [nil]))
          (re-matches #"[+-]?([0-9]+[.])?[0-9]+" statement)
            (append-code `(swap! ~'stack concat ~[statements-group]))
          (re-matches #"<pop>" statement)
            (append-code `(swap! ~'stack butlast))
          (re-matches #"![a-zA-Z0-9]+" statement)
            (append-code 
              `(if (contains? @~'vars ~statement)
                (swap! ~'stack concat [(get @~'vars ~statement)])
                (println "Statement" ~(+ 1 i) ": variable" ~statement "referred but does not exist!")))
          (re-matches #"![a-zA-Z0-9]+\+" statement)
            (let [varname (apply str (butlast statement))]
              (append-code `(swap! ~'vars assoc ~varname (last @~'stack))))
          (re-matches #"\(invoke> (.*)+ [0-9]+\)" statement)
            (let [fname (str (nth statements-group 1))]
              (append-code 
                `(if-let [fname# (resolve (symbol ~fname))]
                  (let [artry# ~(nth statements-group 2)
                        fargs# (take-last artry#  @~'stack)
                        result# (apply fname# (reverse fargs#))]
                    (reset! ~'stack (drop-last artry# @~'stack))
                    (swap! ~'stack concat [result#]))
                  (println "Statement" ~(+ 1 i) ": invalid ~'stack operation"))))
          (re-matches #"\(if>(.*)+\)" statement)
            (let [ifelse-separated (partition-by #(= % 'else>) (rest statements-group))
                  ifcode (first ifelse-separated)
                  elsecode 
                    (if (some #{'else>} statements-group)
                      (last ifelse-separated)
                      '())]
              (append-code 
                `(let [top-value# (last @~'stack)]
                  (swap! ~'stack butlast)
                  (if top-value#
                    ~(generate-code ifcode)
                    ~(generate-code elsecode)))))
          (re-matches #"<printstack>" statement)
            (append-code `(println  @~'stack))
          (re-matches #"<printvars>" statement)
            (append-code `(println @~'vars))
          (re-matches #"(.*)" statement)
            (append-code `(swap! ~'stack concat ~[statement]))
          :else
            (append-code `(println "Statement" ~(+ 1 i) ": operation unrecognized!"))))) statements))
    @generated-code))

(defmacro defstackfn
  "Define a stack function written in m-MicroLang, given its name, arguments,
  and code statements"
  [fname fargs & statements]
    (if (and (re-matches #"[a-zA-Z]+(.*)" (str fname))
            (re-matches #"\[(.*)\]" (str fargs)))
      (let [fargs-names (if (= fargs '[]) '() (map str fargs))
            functionBody 
             `(if (= (count '~fargs-names) (count ~'fargs-values))
                (let [~'stack (atom '())
                      ~'vars (atom (zipmap '~fargs-names ~'fargs-values))]
                  ~(generate-code statements)
                  (last @~'stack))
                (println "Error: wrong values passed to function!"))]
         `(defn ~fname [& ~'fargs-values] ~functionBody))
      (println "Error in function declaration!")))