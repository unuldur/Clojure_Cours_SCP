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

(def zebre-var #{:bleu :jaune :orange :rouge :vert
                 :anglais :espagnol :japonais :ukrainien
                 :chien :cheval :escargot :renard :zebre
                 :cafe :eau :the :vin
                 :chesterfield :cravens :gitanes :kools :old_golds})

(def zebre-doms (reduce (fn [res i] (assoc res (mkvar "v" i) zebre-var))
                {}
                (range 1 6)))

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
                                                [:rouge   :norvegien :lait :vert :jaune :espagnol :ukrainien :japonais
                                                 :old_golds :gitanes :chesterfield :kools :bleu :orange]
                                                [:anglais :v1        :v3   :cafe :kools :chien    :the       :cravens
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
;;La fonction generate and test prend trois parametre :


(defn choix-variable [doms] (ffirst doms))

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
    (let [x (choix-variable doms)]
      (loop [xdom (doms x)]
        (if (seq xdom)
          (let [xval (first xdom)]
            (if-let [sol' (generate-and-test constraints
                                             (dissoc doms x)
                                             (assoc sol x xval))]
              sol'
              (recur (rest xdom))))
          nil)))))


;;Todo **Exercice** : choix de la variable a plus petit domaine

;;Todo **Exercice** : generateur paresseux de toutes les solutions du probleme sans consommation de pile

;;####################################################################################
;;#############   AC3                          #######################################
;;####################################################################################

;;1.Le support pour une variable x
;; de type xvar1 (:var1 ou var2) et la valeur xval
;;(dans le domaine de la variable), le domaine de la variable y (l'autre variable) et la contrainte concernée

(defn check-constraint
  "docstring"
  [contrainte xvar xval yval]
  (if (= xvar contrainte)
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
    (let [yvar (ovar xvar)]
      (if (contains? (get doms (get constraint yvar)) yval)
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
              (recur (rest xdom) (update doms x (fn [xdom] (disj xdom val))) supp' true))
            ;;deja dans le cache, rien a faire pour xval
            (recur (rest xdom) doms supp' changed)))
        [changed, x, doms, supp]))))