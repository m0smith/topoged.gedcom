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

(def gen-name (gen/not-empty gen/string-alpha-numeric))

(def gen-person
  (gen/tuple gen-id gen-name gen-name))

(def gen-header (gen/return (str "0 HEAD" \newline)))

(defn gen-indis [persons0]
  (gen/fmap (fn [persons]
              (for [[id given-name surname :as person] persons]
                (str "0 "id " INDI " \newline
                     "1 NAME " given-name " /" surname "/" \newline
                     "2 GIVN " given-name  \newline
                     "2 SURN " surname  \newline)))
            (gen/return persons0)))

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

(defn gen-it [persons]
  (gen/fmap (fn [[h p f]]
              [(apply str h (apply str p) (apply str f))
               persons])
            (gen/tuple gen-header
                       (gen-indis persons)
                       (gen/list (gen-fam persons)))))
(def gen-gedcom*
  (gen/bind  (gen/list gen-person)
             gen-it))

(def gen-gedcom
  (gen/fmap (fn [[rtnval persons]] 
              (let [lines (lines-seq (string/split rtnval #"[\n\r\f]+"))]
              [(gedcom-seq lines)
               lines
               persons]))
            gen-gedcom*))
             

(defn tag-from-line [line]
  (let [[level p1 p2] (string/split line #" ")]
    (if (= level "0")
      (or p2 p1) 
      p1)))

(defn compare-record [rec line]
  (is (= (name (:tag rec)) (tag-from-line line))))
  
;; Properties
;; * start with a HEAD record
;; * all 0 level records tags match the input

(ct/defspec test-gedcom-seq 100
  (prop/for-all [[rtnval lines persons] gen-gedcom]
                (is (= :HEAD (:tag( first rtnval))))
                (every? identity
                        (map compare-record 
                             rtnval 
                             lines))))
            


