(ns delta-green.core
  (:gen-class))

(defn gen-entry
  [macro-options [key default]]
  `(~key (if (get ~macro-options ~key)
           (get ~macro-options ~key)
           ~default)))


(defmacro defstats
  [name & defaults]
  (let [groups (map (fn [[opt default]] `(list ~opt ~default))
                    (partition 2 defaults))
        macro-options (gensym 'options)
        macro-options-final (gensym 'options-final)
        macro-option (gensym 'option)
        macro-default (gensym 'default)]
    `(defn ~name [~macro-options]
       (reduce (fn [~macro-options-final [~macro-option ~macro-default]]
                 (if (get ~macro-options-final ~macro-option)
                   ~macro-options-final
                   (assoc ~macro-options-final ~macro-option ~macro-default)))
               ~macro-options
               (list ~@groups)))))


(defstats stats
  :str 0
  :con 0
  :dex 0
  :int 0
  :pow 0
  :cha 0)

(defn stat-as-skill
  [stats key]
  (* 5 (key stats)))

(defn hp-max [core-stats] (int (Math/ceil (/ (+ (:str core-stats)
                                                (:con core-stats))
                                             2))))

(defn wp-max [core-stats] (:pow core-stats))
(defn san-max [core-stats] (* 5 (:pow core-stats)))
(defn bond-max [core-stats] (:cha core-stats))
(defn bp [core-stats] (- (san-max core-stats) (:pow core-stats)))

(defstats skills
  :accounting 10
  :alertness 20
  :anthropology 0
  :archeology 0
  :art 0
  :artillery 0
  :athletics 30
  :bureaucracy 10
  :computer-science 0
  :craft 0
  :criminology 10
  :demolitions 0
  :disguise 10
  :dodge 30
  :drive 20
  :firearms 20
  :first-aid 10
  :forensics 0
  :heavy-machinery 10
  :heavy-weapons 0
  :history 10
  :humint 10
  :law 0
  :medicine 0
  :melee-weapons 30
  :military-science 0
  :navigate 10
  :occult 10
  :persuade 20
  :pharmacy 0
  :pilot 0
  :psychotherapy 10
  :ride 10
  :science 0
  :search 20
  :sigint 0
  :stealth 10
  :surgery 0
  :survival 10
  :swim 20
  :unarmed-combat 40
  :unnatural 0)

(def character-def
  {:personal {:name "Mark Brown"
              :profession "Historian"
              :employer "University"
              :nationality "American"
              :sex :m
              :age 30
              :education "PhD History - focus on the Occult"}
   :stats {:str 10
           :con 10
           :dex 10
           :int 17
           :pow 13
           :cha 12}
   :skills (skills {:anthropology 50
                   :bureaucracy 40
                   :history 60
                   ;; field of study
                   :occult (+ 40 20)
                   :persuade 40
                   :archeology 40
                   :search 60
                   :languages {:old-norse 50
                               :old-gaelic 40
                               :latin (+ 0 20 20)
                               :sumerian (+ 0 20)}

                   ;; HEMA
                   :melee-weapons (+ 30 20)
                   :dodge (+ 30 20)
                   :alertness (+ 20 20)

                   :craft {:blacksmithing (+ 0 20)}

                   })
   :bonds {:grad-school {:description "Best friend from grad school"
                         :start 12}
           :hema {:description "HEMA instructor/Medieval studies professor"
                  :start 12}
           :dept-head {:description "Head of department"
                       :start 12}
           :therapist {:description "Therapist"
                       :start 12}}

   :motivations ["The need to know the truth"
                 "Proving his field is valid"
                 "Professionalism"
                 "Protect humanity from the unnatural"
                 "Solace - blacksmithing"]})

(defn inital-status
  [character]
  (let [s (:stats character)
        bonds (:bonds character)]
    {:hp (hp-max s)
     :san (san-max s)
     :wp (wp-max s)
     :bonds (into {} (mapv (fn [[b info]] [b (:start info)]) bonds))
     :adaptation {:violence 0
                  :helplessness 0}
     :failed-skills {}}))

(def history-file "history.log")
(def history (atom (load-file history-file)))

(defn backup
  []
  (spit (str history-file ".backup") @history))

(backup)

(defn save
  []
  (spit history-file @history))

(defn undo
  []
  (swap! history drop-last))

(def character (atom {:def character-def
                      :status (inital-status character-def)}))

(defn emit-event
  [event]
  (swap! history #(conj % event)))
(defn clear-history [] (swap! history (fn [_] [])))

(def history-registry (atom {}))
(defn register
  [key f]
  (swap! history-registry assoc key f))

(defn to-event
  [n args]
  (let [full-args (conj args 'comment)]
    (apply merge (concat (map (fn [arg] {(keyword (name arg)) arg}) full-args)
                         {:type (keyword (name n))}))))

(defmacro with-let
  [event args exprs]
  (if-let [arg (first args)]
    `(let [~arg (~(keyword (name arg)) ~event)]
       (with-let ~event ~(rest args) ~exprs))
    `(do ~@exprs)))

(defmacro defmod
  [n args & apply-exprs]
  (let [event (to-event n args)
        event-sym (gensym 'event)
        history-key (:type event)
        apply-n (symbol (str "apply-" (name n)))
        result (gensym 'result)
        comment 'comment]
  `(list (defn ~apply-n
           [~event-sym]
           (with-let ~event-sym ~args ~apply-exprs))
         (register ~history-key ~apply-n)
         (defn ~n
           ([~@args ~comment]
            (let [~result (~apply-n ~event)]
              (emit-event ~event)
              ~result))
           ([~@args]
            (~n ~@args {}))))))

(defn modify-path
  [f & full-path]
  (let [current-value (reduce get @character full-path)
        new-value (f current-value)]
    (swap! character assoc-in full-path new-value)))

(defmod damage
  [stat amount]
  (modify-path #(- % amount) :status stat))

(defn heal
  ([stat amount comment]
   (damage stat (- amount) comment))
  ([stat amount]
   (heal stat amount {})))

(defn restore
  ([stat stat-max comment]
  (let [status (->> @character :status)
        stats (->> @character :def :stats)
        delta (- (stat-max stats) (stat status))]
    (if (> delta 0)
      (heal stat delta comment)
      nil)))
  ([comment]
   (do (restore :hp hp-max comment)
       (restore :wp wp-max comment))))

(defmod damage-bond
  [bond amount]
  (modify-path #(- % amount) :status :bonds bond))

(defn get-status
  []
  (let [stats (->> @character :def :stats)
        status (:status @character)]
    (println "hp" (:hp status) "/" (hp-max stats))
    (println "wp" (:wp status) "/" (wp-max stats))
    (println "san" (:san status) "/" (san-max stats))
    (println "bp" (bp stats))))

(defn get-bonds
  []
  (let [bond-max (->> @character :def :stats bond-max)
        bonds (get-in @character [:status :bonds])
        max-length (apply max (map (fn [[b _]] (count (str b))) bonds))]
    (doseq [[bond v] bonds]
      (printf (str "%-" max-length "s %d / %d\n") bond v bond-max))))

(defn get-skills
  [& skills]
  (-> @character :def :skills (select-keys skills)))

(defn roll-percent
  []
  (rand-int 100))

(defn roll-dn
  [n]
  (inc (rand-int n)))

(defn roll-6
  []
  (roll-dn 6))

(defn roll-10
  []
  (roll-dn 10))

(defn check
  [modifier & path]
  (let [target-value (modifier (get-in @character path))
        roll (roll-percent)]
    (if (< roll target-value)
      (do (printf "Success %d vs %d\n" roll target-value)
          true)
      (do (printf "Failed %d vs %d\n" roll target-value)
          false))))

(defn inc-map-key
  [key & path]
  (apply modify-path
         #(assoc % key (if-let [current (get-in @character (conj (vec path) key))]
                         (inc current)
                         1))
         path))

(defmod fail-skill-check
  [skill]
  (inc-map-key skill :status :failed-skills))

(defn skill-check
  ([skill options]
   (let [advantage (if-let [a (:adv options)] a 0)
         disadvantage (if-let [d (:dis options)] d 0)
         modifier #(+ % (- advantage disadvantage))]
     (if (check modifier :def :skills skill)
       (->> @character :status :failed-skills)
       (do
         (fail-skill-check skill (:comment options))
         (->> @character :status :failed-skills)))))
   ([skill] (skill-check skill {})))

(def stat-check (partial check #(* 5 %) :def :stats))
(defn san-check [] (check identity :status :san))

(defmod skill-change
  [skill amount]
  (modify-path #(+ % amount) :def :skills skill))

(defmod learn
  []
  (do (doseq [[failed-skill _] (->> @character :status :failed-skills)]
        (modify-path inc :def :skills failed-skill))
      (modify-path (fn [_] {}) :status :failed-skills)))

(defmod adapt
  [kind]
  (inc-map-key kind :status :adaptation))

(def adapt-to-violence (partial adapt :violence))
(def adapt-to-helplessness (partial adapt :helplessness))

(defn get-adaptation
  []
  (get-in @character [:status :adaptation]))

(defmod end-session
  []
  nil)

(defn apply-history
  [history]
  (let [event (first history)
        apply-event (get @history-registry (:type event))]
    (cond (not event) nil
          (not apply-event) (print "Unknown event type" event)
          :else (do (apply-event event)
                    (apply-history (rest history))))))

(apply-history @history)

