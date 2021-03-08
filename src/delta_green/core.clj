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

(defn wp [core-stats] (:pow core-stats))
(defn san-max [core-stats] (* 5 (:pow core-stats)))
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
   :bonds [["Best friend from grad school" 12]
           ["HEMA instructor/Medieval studies professor" 12]
           ["Head of department" 12]
           ["Therapist" 12]]

   :motivations ["The need to know the truth"
                 "Proving his field is valid"
                 "Professionalism"
                 "Protect humanity from the unnatural"
                 "Solace - blacksmithing"]})

(defn inital-status
  [character]
  (let [s (:stats character)]
    {:hp (hp-max s)
     :san (san-max s)
     :failed-skills #{}}))

(def character (atom {:def character-def
                      :status (inital-status character-def)}))

(def history (atom []))
(defn emit-event
  [event]
  (swap! history #(conj % event)))
(defn clear-history [] (swap! history (fn [_] [])))

(defn modify-path
  [f & full-path]
  (let [current-value (reduce get @character full-path)
        new-value (f current-value)]
    (swap! character assoc-in full-path new-value)))

(defn damage
  [stat amount]
  (do (modify-path #(- % amount) :status stat)
      (emit-event ``(damage ~~stat ~~amount))))

(defn heal
  [stat amount]
  (damage stat (- amount)))

(defn roll-percent
  []
  (rand-int 100))

(defn roll-6
  []
  (inc (rand-int 6)))

(defn check
  [modifier & path]
  (let [target-value (modifier (get-in @character path))
        roll (roll-percent)]
    (if (< roll target-value)
      (do (printf "Success %d vs %d\n" roll target-value)
          true)
      (do (printf "Failed %d vs %d\n" roll target-value)
          false))))

(defn skill-check
  [skill]
  (if (check identity :def :skills skill)
    @character
    (when-not ((reduce get @character [:status :failed-skills]) skill)
      (emit-event ``(mark ~~skill))
      (modify-path #(conj % skill) :status :failed-skills))))

(def stat-check (partial check #(* 5 %) :def :stats))
(defn san-check [] (check identity :status :san))

(defn end-session
  []
  (do (doseq [failed-skill (->> @character :status :failed-skills)]
        (modify-path inc :def :skills failed-skill))
      (modify-path (fn [_] #{}) :status :failed-skills)))


