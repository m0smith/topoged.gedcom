(ns topoged.gedcom-test
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [topoged.gedcom :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as ct]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))


(def gen-id 
  (gen/fmap (fn [f] (str "@" f "@"))
        (gen/not-empty gen/string-alpha-numeric)))

(def gen-name-value (gen/not-empty gen/string-alpha-numeric))

(defrecord LevelN [level v1 v2])

(defn new-level [level tag id]
  (if (= level 0)
    (->LevelN "0" id tag)
    (->LevelN (str level) tag id)))

(defn levelstr [{:keys [level v1 v2]}]
  (str level " " v1 " " v2 \newline))


(def gen-person
  (gen/tuple gen-id gen-name-value gen-name-value))

(def gen-header (gen/return (str "0 HEAD" \newline)))

(defn gen-level
  ""
  [level tag value-gen]
  (gen/fmap (partial new-level level tag)
            value-gen))

(defn gen-name-det "" [[given surname]]
  (gen/tuple
   (gen-level 1 "NAME" (gen/return (str given " /" surname "/")))
   (gen-level 2 "GIVN" (gen/return given))
   (gen-level 2 "SURN" (gen/return surname))))

(def gen-name
  ""
  (gen/bind (gen/tuple gen-name-value gen-name-value)
            gen-name-det))

(defn gen-indi 
  ""
  [[id given surn :as person]]
  (gen/tuple
   (gen-level 0 "INDI" (gen/return id))
   (gen/list gen-name)))
   

(defn gen-indis [persons0]
  (apply gen/tuple (map gen-indi persons0)))

(defn random-fam [persons]
  (let [family (take (rand-int (count persons)) (shuffle persons))
        labels (concat [ "HUSB" "WIFE" ] (repeat "CHIL"))]
    (apply str (map #(str "1 " %1 " " %2 \newline) labels (map first family)))))
    

(defn gen-fam [persons0]
  (gen/fmap (fn [[fam-id persons]]
              (apply str (str "0 " fam-id " FAM " \newline)
                      (random-fam persons)))
            (gen/tuple
             gen-id 
             (gen/return persons0))))

(defn levels->str [levels]
  (->> levels
       flatten
       (map levelstr)
       (apply str)))

(defn gen-it [persons]
  (gen/fmap (fn [[h p f]]
              [(apply str h 
                      (levels->str p) 
                      (apply str f))
               persons])
            (gen/tuple gen-header
                       (gen-indis persons)
                       (gen/list (gen-fam persons)))))
(def gen-gedcom*
  (gen/bind (gen/list gen-person)
            gen-it))

(def gen-gedcom
  (gen/fmap (fn [[rtnval persons]] 
              (let [lines (lines-seq (string/split rtnval #"[\n\r\f]+"))]
              [(gedcom-seq lines)
               lines
               persons]))
            gen-gedcom*))
             

(defn tag-from-line [part]
  (let [[level p1 p2] (string/split (first part) #" ")]
    (if (= level "0")
      (or p2 p1) 
      p1)))

(defn compare-record [rec part]
  (is (= (name (:tag rec)) (tag-from-line part))))
  
;; Properties
;; * start with a HEAD record
;; * all 0 level records tags match the input

(ct/defspec test-gedcom-seq 50
  (prop/for-all [[rtnval lines persons] gen-gedcom]
                (is (= :HEAD (:tag( first rtnval))))
                (let [parts (partition-starting-every (level? \0) lines)]
                  ;(println parts)
                  (every? identity
                          (map compare-record 
                               rtnval 
                               parts)))))
                             
            


