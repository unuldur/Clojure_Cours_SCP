(ns contraintes.td5)


;;####################################################################################
;;#############   REPRESENTATION DE CSP        #######################################
;;####################################################################################
;; les variables : utilisation des keywords
;; les domaines : des ensembles discret et bornés, utilisation des ensemble
#{:rouge :vert :bleu}

;;Comment relier les variables au domaine : les maps
(def cercles-doms {:v1 (into #{} (range -1 15))
                   :v2 (into #{} (range -5 10))})

;; les contraintes binaires : un prédicat:
(defn sq [x] (* x x))
(defn c1-check [x y]
  (<= (+ (sq (- x 9)) (sq y))
      25))

(defn c2-check [x y]
  (<= (+ (sq (+ x 1)) (sq (- y 5)))
      100))

;;Les contraintes identifient les variables :

(def c1 {:var1 :v1
         :var2 :v2
         :check c1-check})

(def c2 {:var1 :v1
         :var2 :v2
         :check c2-check})

(def cercles-constraint [c1 c2])

;;####################################################################################
;;#############         COLORIAGE DE CARTE     #######################################
;;####################################################################################

(defn mkvar [prefix num]
  (keyword (str prefix num)))

(def coloriage-doms (reduce (fn [res i] (assoc res (mkvar "v" i) #{:bleu :rouge :vert}))
                            {}
                            (range 1 8)))


(def coloriage-constraints (mapv (fn [x y] {:var1 (mkvar "v" x)
                                           :var2 (mkvar "v" y)
                                           :check not=})
                                [1 1 2 2 3 3 3 4 5 6]
                                [2 3 3 4 4 5 5 5 6 7]))


;;Todo **Exercice** : représentation du probléme du zèbre en Clojure
;;####################################################################################
;;#############         ZEBRE                  #######################################
;;####################################################################################


(def zebre-doms (reduce (fn [res i] (case i
                                      :norvegien (assoc res i #{1})
                                      :lait (assoc res i #{3})
                                      (assoc res i #{1 2 3 4 5})))
                        {}
                        #{:bleu :jaune :orange :rouge :vert
                          :anglais :espagnol :japonais :ukrainien
                          :chien :cheval :escargot :renard :zebre
                          :cafe :eau :the :vin
                          :chesterfield :cravens :gitanes :kools :old_golds :norvegien :lait}))

(defn dif-all-constrainte
  "retourne toute les contraintes de difference de `v`"
  [v]
  (loop [v v res []]
    (if (seq v)
      (recur (rest v) (loop [v2 (rest v) res2 res]
                                  (if (seq v2)
                                    (recur (rest v2) (conj  res2 {:var1 (first v)
                                                                  :var2 (first v2)
                                                                  :check not=}))
                                    res2)))
      res)))

(defn neighbour
  [var1 var2]
  (= (Math/abs (- var1 var2)) 1))

(defn left
  "test si `var1` et a gauche de `var2`"
  [var1 var2]
  (= var1 (inc var2)))

(def zebre-constraints (clojure.set/union (mapv (fn [x y z] {:var1 x
                                                           :var2 y
                                                           :check z})
                                                [:rouge :vert :jaune :espagnol :ukrainien :japonais
                                                 :old_golds :gitanes :chesterfield :kools :bleu :orange]
                                                [:anglais :cafe :kools :chien    :the       :cravens
                                                 :escargot  :vin :renard :cheval :norvegien :vert]
                                                [=        =          =     =      =     =          =          =
                                                 =         = neighbour neighbour left left])
                                          (dif-all-constrainte [:bleu :jaune :orange :rouge :vert])
                                          (dif-all-constrainte [:anglais :espagnol :japonais :norvegien :ukrainien])
                                          (dif-all-constrainte [:chien :cheval :escargot :renard :zebre])
                                          (dif-all-constrainte [:cafe :eau :lait :the :vin])
                                          (dif-all-constrainte [:chesterfield :cravens :gitanes :kools :old_golds])))

;;####################################################################################
;;#############   FONCTION  GENERATE AND TEST  #######################################
;;####################################################################################


(defn choix-variable [doms] (ffirst doms))
(defn choix-variable-min [doms] (first (apply min-key #(count %) doms)))

(defn test-solution [constraints sol]
  (every?
    (fn [constraint] (let [{:keys [check var1 var2]} constraint] (check (sol var1) (sol var2))))
    constraints))

(defn generate-and-test
  "la map des domaines `doms`, le vecteur des contraintes `constraints`, optionellement une solution partielle `sol`"
  [constraints doms sol]
  (if (empty? doms)
    (if (test-solution constraints sol)
      sol
      nil)
    ;;sol incomplete
    (let [x (choix-variable-min doms)]
      (loop [xdom (doms x)]
        (if (seq xdom)
          (let [xval (first xdom)]
            (if-let [sol' (generate-and-test constraints
                                             (dissoc doms x)
                                             (assoc sol x xval))]
              sol'
              (recur (rest xdom))))
          nil)))))

(let [stack []]
  )


;;Todo **Exercice** : generateur paresseux de toutes les solutions du probleme sans consommation de pile
(defn create_vector
  [key vals]
  (reduce (fn [res val] (conj res {key val})) [] vals))

;;(defn generate-all-sol
;;  "docstring"
;;  [constraints doms sol stack]
;;  (if (and (empty? stack) (empty? doms))
;;    ()
;;    (loop [doms' doms stack stack res stack]
;;      (if (seq doms')
;;        (let [[key val] (first stack)
;;              doms'' (dissoc doms' key)]
;;          (recur doms'' (concat stack (create_vector key (second (first doms'')))) (assoc res key val)))
;;        (let [newdoms ()])
;;        (if (test-solution constraints sol)
;;          (lazy-seq (cons res (generate-all-sol ))))))))

;;####################################################################################
;;#############   AC3                          #######################################
;;####################################################################################

;;1.Le support pour une variable x
;; de type xvar1 (:var1 ou var2) et la valeur xval
;;(dans le domaine de la variable), le domaine de la variable y (l'autre variable) et la contrainte concernée

(defn check-constraint
  "docstring"
  [contrainte xvar xval yval]
  (if (= xvar :var1)
    ((:check contrainte) xval yval)
    ((:check contrainte) yval xval)))

(let [const {:var1 :r4
             :var2 :m
             :check =}]
  (check-constraint const :var1 4 4))

(defn support
  [xvar xval ydom contrainte]
  (reduce (fn [res yval]
            (if (check-constraint contrainte
                                 xvar
                                 xval
                                 yval)
              (reduced yval)
              res)) nil ydom))

(let [const {:var1 :r4
             :var2 :m
             :check =}]
  (support :var2 4 #{1 2 3 4 5} const))

;;revision des domaines

(declare check-cache)
(declare update-support)

(defn ovar
  "docstring"
  [xvar]
  (if (= xvar :var1)
    :var2
    :var1))


(defn check-cache
  "docstring"
  [constraint supp doms const-ref xvar x xval]
  (if-let [yval (get (nth supp const-ref) [x xval])]
    (let [yvar (ovar xvar), y (get constraint yvar)]
      (if (contains? (get doms y) yval)
          [false, supp]
          [true, (update supp const-ref (fn [old] (dissoc old [x xval])))]))
    ;;rien dans le cache
    [true, supp]))

(defn update-support
  "docstring"
  [supp const-ref x xval y yval]
  (-> supp
      (update const-ref (fn [xsupp] (assoc xsupp [x xval] yval)))
      (update const-ref (fn [ysupp] (assoc ysupp [y yval] xval)))))

(defn revise
  "`constraints` : un vecteur de contraintes
    `doms` : domaines des variables
    `supps` : cache des supports, un vecteur de map  {[x val} yval}
            ex : [{[:v1 12] 34, [:v1 14] 84}, ]
    `contraint-ref` : le numero de la contrainte concernée
    `xvar` : la variable concernée
    la fonction doit retourner un quadruplet [changed, x, doms, supp]
    avec  : changed : vaut true si il y a eu une revision pour x
            x : le nom de la variable
            doms : les domaines
            supp: le cache des support"
  [constraints doms supps contraint-ref xvar]
  (let [contraint (nth constraints contraint-ref)
        x (get contraint xvar)
        y (get contraint (ovar xvar))]
    (loop [xdom (get doms x), doms doms,supp supps, changed false]
      (if (seq xdom)
        (let [xval (first xdom)
              [findnew, supp'] (check-cache contraint supp doms contraint-ref xvar x xval)]
          (if findnew
            ;;il faut trouver un nouveau support
            (if-let [yval (support xvar xval (get doms y) contraint)]
              (recur (rest xdom) doms (update-support supp' contraint-ref x xval y yval) changed)
              ;;pas de support pour x et xval
              (recur (rest xdom) (update doms x (fn [xdom] (disj xdom xval))) supp' true))
            ;;deja dans le cache, rien a faire pour xval
            (recur (rest xdom) doms supp' changed)))
        [changed, x, doms, supp]))))

(defn init-support
  "initialiser le cache"
  [constraint]
  (mapv (fn [_] {}) (range (count constraint))))


;;EXEMPLE ::
(let [contraints [{:var1 :v :var2 :w :check (fn [b n] (= b (+ n 1)))}]]
  (revise contraints {:v #{1 2 3 4 5} :w #{1 2 3 4 5}} (init-support contraints) 0 :var1))

(let [contraints [{:var1 :v :var2 :w :check (fn [b n] (= b (+ n 1)))}]]
  (revise contraints {:v #{1 2 3 4 5} :w #{1 2 3 4 5}} (init-support contraints) 0 :var2))

;;######### AC3 ##############
(declare init-todo)
(declare select-todo)
(declare update-todo)
(defn ac3
  [constraints doms]
  (let [supp (init-support constraints)
        todo (init-todo constraints)]
    (loop [todo todo, supp supp, doms doms]
      (if (seq todo)
        (let [[const-ref, xvar, todo'](select-todo constraints doms todo)
              [changed, x, doms' supp'](revise constraints doms supp const-ref xvar)]
          (if changed
            (if (seq (get doms' x))
              (recur (update-todo constraints const-ref xvar x todo') supp' doms')
              ;;plus de valeur pour x
              nil)
            ;;pas de changement dans le domaine de x
            (recur todo' supp' doms')))
        ;; todou vide
        doms))))

(defn init-todo
      [constraints]
  (loop [const-ref 0, todo #{}]
    (if (< const-ref (count constraints))
      (recur (inc const-ref) (conj todo [const-ref :var1] [const-ref :var2]))
      todo)))

(defn select-todo [constraints doms todo]
  (let [[const-ref, xvar] (first todo)]
    [const-ref, xvar, (rest todo)]))

;;TODO **EXERCICE** Proposer une autre heuristique pour la selection d'un autre couple variable et comparer les performances pour le zebre

(defn update-todo
  [constraints prev-cref xvar x todo]
  (loop [const-ref 0, todo todo]
    (if (< const-ref (count constraints))
      (let [constraint (nth constraints const-ref)]
        (if (and (= const-ref prev-cref) (= (get constraint xvar) x))
          (let [yvar (ovar xvar)]
            (recur (inc const-ref) (conj todo [const-ref yvar])))
          ;;autre cas
          (recur (inc const-ref) (cond
                                   (= (:var1 constraint) x) (conj todo [const-ref :var2])
                                   (= (:var2 constraint) x) (conj todo [const-ref :var1])
                                   :else todo))))
      todo)))


;;TODO **Exercice** :
;;faires des variantes des algorithmes generate and test
;;(une solution ou toutes les solutions en lazy) pour integrer ac3


;;####################################################################################
;;#############   graphes                      #######################################
;;####################################################################################

(def graph1
  {:A #{:B :D}
  :B #{:C}
  :C #{:F}
  :D #{:E}
  :E #{:B :F}
  :F #{}})

(defn add-vertex
  "docstring"
  [graph id]
  (if (get graph id)
    graph
    (assoc graph id #{})))

(defn add-edge
  [graph src dst]
  (update graph src (fn [dests] (conj dests dst))))


;; Exercice fonction all-edges qui retourne la sequence paresseuese des arretes (couples [src dst]) du graphe
;; exemple : (all-edges graph1) -> ([:A :B][:A :D] etc...)

(defn all-edges
  [graph]
  (if (seq graph)
    (let [[src, dsts] (first graph)]
      (lazy-cat (map (fn[dst] [src, dst]) dsts) (all-edges (rest graph))))
    ()))


;;Exercice fonction transpose qui construit le graphe transposé
(defn transpose
  [graph]
  (loop [edges (all-edges graph) res {}]
    (if (seq edges)
      (let [[src, dst] (first edges)]
        (recur (rest edges) (-> res
                                (add-vertex src)
                                (add-vertex dst)
                                (add-edge dst src))))
      res)))

;;Exercice: une fonction dfs (parcours en profondeur)
;;par exemple:
;; (dfs graph1 :A (fn [vert] 1) + 0) ~> 6
;; (dfs graph1 :A identity conj []) ~> [:A :D :E :B :C :F]

(defn dfs
  [graph vert fvert facc init]
  (loop [stack (list vert), visited #{}, acc init]
    (if (seq stack)
      (let [acc' (facc acc (fvert (first stack)))
            visited' (conj visited (first stack))
            succs (clojure.set/difference (get graph (first stack)) visited')]
        (recur (distinct (concat succs (rest stack))) visited' acc'))
      acc)))

;;Exercice: une fonction bfs (parcours en largeur)
;;par exemple:
;; (bfs graph1 :A (fn [vert] 1) + 0) ~> 6
;; (bfs graph1 :A identity conj []) ~> [:A :D :B :E :C :F]

(defn bfs
  [graph vert fvert facc init]
  (loop [stack (list vert), visited #{}, acc init]
    (if (seq stack)
      (let [acc' (facc acc (fvert (first stack)))
            visited' (conj visited (first stack))
            succs (clojure.set/difference (get graph (first stack)) visited')]
        (recur (distinct (concat (rest stack) succs)) visited' acc'))
      acc)))

;;Composantes fortement connexes d'un graphe dirigé

(def graph2 {:A #{:F :B}
             :F #{:G :H}
             :B #{:C}
             :G #{:H :I}
             :H #{:I :F}
             :I #{:E}
             :E #{:C}
             :C #{:D}
             :D #{:E}
             :J #{}})


;;Algo : dfs g
;;stack     : ()
;;#visited  : {G H F I E C D J}
;;acc       : (G H J I E C D F)
;;dfs a (A B G H J I E C D F)
;;


;;####################################################################################
;;#############   couplages maximal            #######################################
;;####################################################################################


(def graph3 {:v1 #{1 2 3}
             :v2 #{1 2 4 5}
             :v3 #{4 5 6}
             :v4 #{4 5 6}
             :v5 #{4 5 6}})

;;couplage maximal du graph3:
(def couplage-max-graph-3 {
                           1 :v2
                           3 :v1
                           4 :v5
                           5 :v3
                           6 :v4
                           })
;;ou nil si il y pas de couplage

(defn augment [bigraph src visited match]
  (loop [dests (get bigraph src) visited visited match match]
    (if (seq dests)
      (if (visited (first dests))
        (recur (rest dests) visited match)
        ;;pas encore visité
        (if-let [msrc (get match (first dests))]
          (let [[found,visited',match'] (augment bigraph msrc (conj visited (first dests)) match)]
            (if found
              [true, visited', (assoc match' (first dests) src)]
              (recur (rest dests) visited' match')))
          ;;pas encore de match pour (first dests)
          [true, (conj visited (first dests)), (assoc match (first dests) src)]))
      [false, visited, match])))

(defn max-matching
  "defini le couplage maximale d'un graphe"
  [bigraph]
  (loop [verts (keys bigraph) match {}]
    (if (seq verts)
      (let [[_,_,match'] (augment bigraph (first verts) #{} match)]
        (recur (rest verts) match'))
      (if (= (count bigraph) (count match))
        match
        nil))))