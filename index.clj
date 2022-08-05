;; # Analisi COVID-19 Italia

^{:nextjournal.clerk/visibility :hide-ns
  :nextjournal.clerk/toc        true}
(ns index
  (:require [nextjournal.clerk :as clerk]
            [cheshire.core :as json]
            [clojure.data.csv :as csv]
            [clojure.set :as set]))

^{::clerk/visibility :hide
  ::clerk/viewer     clerk/hide-result}
(comment
  (clerk/serve! {:browse? true}))

;; ## Dati

;; Scarichiamo i dati dalle fonti ufficiali della protezione civile.


#_(def data (slurp "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-andamento-nazionale.json"))
#_(def regioni (slurp "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-regioni.json"))
#_(def province (slurp "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-json/dpc-covid19-ita-province.json"))

(do
  (def data (slurp "data.json"))
  (def regions (slurp "data_regioni.json"))
  (def provinces (slurp "data_province.json"))
  (def italy-population (slurp "italy-population.csv"))
  (def national-data (json/parse-string data true))
  (def regional-data (json/parse-string regions true))
  (def provincial-data (json/parse-string provinces true)))


;; ## Esplorando i dataset

;; Diamo ora uno sguardo alle colonne che sono nel nostro dataset.

(sort (keys (first national-data)))

;; Dal momento che ci sono molte colonne a cui non siamo interessati, definiamo un sottoinsieme di esse da usare per filtrare i dati

(def selected-keys
  [:totale_casi
   :dimessi_guariti
   :deceduti
   :tamponi
   :tamponi_test_molecolare
   :totale_ospedalizzati
   :terapia_intensiva
   :ingressi_terapia_intensiva
   :data])

;; Ora, guardiamo i dati dell'ultimo giorno disponibile.

(-> (last national-data)
    (select-keys selected-keys)
    (seq)
    (clerk/table))

;; Filtriamo i dati nazionali con solo le colonne che abbiamo selezionato:

(def filtered-data (distinct (map #(select-keys % selected-keys) national-data)))

;; Nei dati a disposizioni, alcuni valori sono riportati come "giornalieri", altri invece sono
;; "cumulativi". Vediamone un esempio:

(-> (take-last 5 filtered-data)
    (clerk/table))

;; Da questa tabella, vediamo che:

;; - `totale_casi`, `tamponi`, `tamponi_test_molecolare`, `terapia_intensiva`, `totale_ospedalizzati`,  `dimessi_guariti`, `deceduti` sono dati cumulativi
;; - `ingressi_terapia_intensiva` e' un valore incrementale

;; Possiamo manipolare i dati cumulativi (visto che abbiamo i loro valori giorno per giorno) e calcolare noi stessi la differenza fra un giorno e il precedente.
;; Questo e' particolarmente utile per l'analisi dello sviluppo e contenimento della pandemia.

;; Definiamo ora una funzione, `calculate_delta`, che, a partire da una sequenza di dati cumulativi, ne crea una con le differenze giornaliere.

(defn calculate-delta [coll]
  (map (fn [a b]
         (into {} (map (fn [[k1 v1] [_ v2]]
                         (case k1
                           :data [k1 v1]
                           [k1 (when (and v2 v1)
                                 (- v2 v1))]))
                       a b)))
       coll
       (rest coll)))

;; Ridefiniamo ora le colonne a cui siamo interessati, specificando per ognuna qual'e' la colonna di riferimento del dataset originale,
;; e se dobbiamo prenderla cosi' com'e' (`:original`) oppure calcolarne le differenze giornaliere (`:delta`).

(def columns {:totale-casi                      {:source :totale_casi
                                                 :type   :original}
              :nuovi-casi                       {:source :totale_casi
                                                 :type   :delta}

              :deceduti                         {:source :deceduti
                                                 :type   :original}
              :nuovi-deceduti                   {:source :deceduti
                                                 :type   :delta}

              :tamponi                          {:source :tamponi
                                                 :type   :delta}
              :tamponi-pcr                      {:source :tamponi_test_molecolare
                                                 :type   :delta}

              :nuovi-guariti                    {:source :dimessi_guariti
                                                 :type   :delta}

              :totale-ospedalizzati             {:source :totale_ospedalizzati
                                                 :type   :original}

              :terapia-intensiva                {:source :terapia_intensiva
                                                 :type   :original}
              :delta-terapia-intensiva          {:source :terapia_intensiva
                                                 :type   :delta}

              :nuovi-ingressi-terapia-intensiva {:source :ingressi_terapia_intensiva
                                                 :type   :original}

              :data                             {:source :data
                                                 :type   :original}})

;; Adesso creiamo il nostro dataset finale che conterra' queste colonne:

(defn finalize-data [input-data]
  (let [incremental-data (calculate-delta input-data)]
    (into {}
          (for [[column-name {:keys [source type]}] columns]
            [column-name (map source (if (= type :original)
                                       input-data
                                       incremental-data))]))))

(def final-data (finalize-data filtered-data))



;; Diamo uno sguardo a questo dataset finale, partendo dall'ultima data disponibile

(clerk/table {::clerk/opts {:fetch-opts {:n 10}}} (into {} (map (fn [[k v]] [k (reverse v)]) final-data)))


;; Definiamo ora una funzione per calcolare la media mobile a `n` giorni di una sequenza di dati.

(defn moving-average [n coll]
  (let [[n1 n2] (split-with nil? coll)]
    (concat n1 (when-let [[x & xs] (seq n2)]
                 (map #(/ % n)
                      (reductions + (* n x)
                                  (map - xs (concat (repeat n x) xs))))))))


;; Creiamo ora una funzione `plot` da poter usare per creare i grafici

(defn plot [plot-spec opts]
  (clerk/plotly
    {:data   (map (fn [{:keys [key color legend]}]
                    {:name legend
                     :mode "lines"
                     :line {:color color}
                     :x    (map (comp #(apply str %) #(take 10 %))
                                (:data final-data))
                     :y    (cond->> (key final-data)
                                    (:average opts) (moving-average (:average opts)))})
                  plot-spec)
     :layout {:margin {:l 80 :b 50 :r 50 :t 50}
              :xaxis  {:autotick       true
                       :automargin     true
                       :showticklabels true}
              :yaxis  {:automargin true}}}))

;; ## Analisi

;; ### Positivi

;; Creiamo ora un grafico per visualizzare i nuovi positivi, deceduti e guariti giorno per giorno,
;; mostrando la media mobile a 7 giorni che rende i dati piu' fruibili

(plot [{:key :nuovi-casi :color "orchid" :legend "Positivi"}
       {:key :nuovi-deceduti :color "tomato" :legend "Deceduti"}
       {:key :nuovi-guariti :color "limegreen" :legend "Guariti"}]
      {:average 7})


;; ### Tamponi

(plot [{:key :nuovi-positivi :color "orchid" :legend "Positivi"}
       {:key :tamponi :color "darkslateblue" :legend "Tamponi (tot)"}
       {:key #(or (:tamponi-pcr %) (:tamponi %)) :color "darkcyan" :legend "Tamponi PCR"}]
      {:average 7})


;; ### Ospedalizzati e TI

(plot [{:key :totale-ospedalizzati :color "red" :legend "Ricoverati"}
       {:key :terapia-intensiva :color "orange" :legend "Terapia Intensiva"}] {})

(plot [{:key :nuovi-ingressi-terapia-intensiva :color "red" :legend "Ingressi TI"}] {:average 7 :last 60})


;; ## Popolazione

;; Analizziamo ora i dati non in senso assoluto e a livello nazionale, ma andiamo a vedere ora nel dettaglio regioni e
;; provincie.

;; Cominciamo a manipolare il dataset della popolazione italiana, provvedendo anche a rinominare alcuni dati per usare
;; la stessa nomenclatura usata nei dati della protezione civile.

(def population (set/rename-keys (->> (csv/read-csv italy-population)
                                      (drop 1)
                                      (map (fn [[k n]] [k (Integer/parseInt n)]))
                                      (into {}))
                                 {"Valle d'Aosta / Vallée d'Aoste" "Valle d'Aosta"
                                  "Bolzano / Bozen"                "P.A. Bolzano"
                                  "Trento"                         "P.A. Trento"}))


(def zone #{"Italia" "Isole" "Sud" "Nord-ovest" "Nord-est" "Centro"})

(def regioni #{"Valle d'Aosta" "Piemonte" "Liguria" "Lombardia" "Trentino Alto Adige / Südtirol" "Veneto"
               "Friuli Venezia Giulia" "Emilia-Romagna" "Marche" "Toscana" "Umbria"
               "Lazio" "Molise" "Abruzzo" "Campania" "Puglia" "Basilicata" "Calabria"
               "Sicilia" "Sardegna"})

(def province (set/difference (set (keys population))
                              (set/union zone regioni)))


;; Tuttavia, visto che la popolazione italiana non e' uniformemente distribuita fra le varie regioni / province, dobbiamo
;; necessariamente calcolare dati relativi.

;; Guardiamo infatti, ad esempio, la provincia piu' popolosa d'Italia con quella meno.

(let [sorted-pop (->> population
                      (filter (fn [[k _]] (province k)))
                      (sort-by second))]
  [(first sorted-pop)
   (last sorted-pop)])

;; Vista la grande differenza fra queste due province, la prima avra' necessariamente meno casi della seconda,
;; se considerassimo dati assoluti, rendendo pericio' i dati poco confrontabili.

;; I dati relativi alla popolazione sono di solito considerati "per 100.000 abitanti"; calcoliamo ora la popolazione delle province
;; regioni e zone, calcolando "quante centinaia di migliaia di persone" vi abitano.

(def pop-100k
  (into {} (map (fn [[k v]] [k (float (/ v 100000))]) population)))


;; ## Analisi Regioni

(def regions-config
  {"Lombardia"      "black"
   "Emilia-Romagna" "darkgreen"
   "Veneto"         "deepskyblue"
   "Sicilia"        "gold"
   "Piemonte"       "brown"
   "Marche"         "orange"
   "Toscana"        "chocolate"
   "Lazio"          "red"
   "Liguria"        "grey"
   "Campania"       "skyblue"
   "Puglia"         "orchid"})

(def provinces-config
  {"Milano"       "black"
   "Ravenna"      "darkgreen"
   "Forlì-Cesena" "red"
   "Ferrara"      "gold"
   "Sondrio"      "brown"
   "Rimini"       "skyblue"})


(defn region-data [region-name]
  (->> regional-data
       (filter #(= (:denominazione_regione %) region-name))
       (map #(select-keys % selected-keys))
       (finalize-data)
       ))

(def data-by-region
  (->>
    (keys regions-config)
    (map (fn [r] [r (region-data r)]))
    (into {})))

(defn province-data [province-name]
  (->>
    provincial-data
    (filter #(= (:denominazione_provincia %) province-name))
    (map #(select-keys % selected-keys))
    distinct
    (finalize-data)))



(def data-by-province
  (->> (keys provinces-config)
       (map (fn [r] [r (province-data r)]))
       (into {})))

(defn plot-local-data [boundary {:keys [key by-100k? average last] :as _opts}]
  (clerk/plotly
    {:data   (mapv (fn [[group-name color]]
                     (let [data (get (case boundary
                                       :regions data-by-region
                                       :provinces data-by-province
                                       nil)
                                     group-name)]
                       {:name group-name
                        :mode "lines"
                        :line {:color color}
                        :x    (cond->> (map (comp #(apply str %) #(take 10 %))
                                            (:data data))
                                       last (take-last last))
                        :y    (cond->> (key data)
                                       by-100k? (map (fn [x] (when x (/ x (get pop-100k group-name)))))
                                       average (moving-average average)
                                       last (take-last last))}))
                   (case boundary
                     :regions regions-config
                     :provinces provinces-config
                     nil))
     :layout {:xaxis {:autotick   true
                      :automargin true}}}))

(plot-local-data :regions
                 {:key      :nuovi-casi
                  :by-100k? true
                  :average  7
                  :last     90})

(plot-local-data :regions
                 {:key      #(or (:tamponi_test_molecolare %) (:tamponi %))
                  :by-100k? true
                  :average  7
                  :last     90})

(plot-local-data :regions
                 {:key      :totale-ospedalizzati
                  :by-100k? true
                  :average  1
                  :last     60})

(plot-local-data :regions
                 {:key      :terapia-intensiva
                  :by-100k? true
                  :average  1
                  :last     60})

(plot-local-data :regions
                 {:key      :nuovi-ingressi-terapia-intensiva
                  :by-100k? true
                  :average  7
                  :last     180})

;; ## Analisi Province

(plot-local-data :provinces
                 {:key      :nuovi-casi
                  :by-100k? true
                  :average  7
                  :last     120})


(java.util.Date.)