(ns topoged.gedcom
  (:use [ clojure.java.io :only [reader]])
  (:require [clojure.string :as str]
            [clojure.pprint :refer :all])
  )


(set! *warn-on-reflection* true)

(defn partition-starting-every
  "Partition the sequence starting each partition when the f is true.  Thanks to Chouser http://groups.google.com/group/clojure/msg/9ec39ef07c92787b"
  [f coll]
  (lazy-seq
    (when-let [[x & xs] (seq coll)]
      (let [[a b] (split-with (complement f) xs)]
        (cons (cons x a) (partition-starting-every f b)))))) 


(defn gedcom?
  "Return true if the string resembles a gedcom file by looking for '0 HEAD' as the first 6 charaters"
  [f]  (if (and (string? f) (> (count f) 6))
         (re-matches #"0 [A-Z][A-Z][A-Z][A-Z]" (subs f 0 6)))
  )

(defn level? [level] (fn [coll] (= level (first coll))))

(defmulti conc-cont*
  "A multi-method that returns the value of a continuing record CONC or CONT"
  (fn [rec] (:tag rec)))
(defmethod conc-cont* :CONC [rec] (:value rec))
(defmethod conc-cont* :CONT [rec] (str \newline (:value rec)))
(defmethod conc-cont* :default [rec] nil)

(defn conc-cont
  "Update rec with CONT and CONC records by adding their content to the :value tag
   and removing them from the :content vector.

   Returns the updated rec."
  [rec]
  (if-let [cont-recs (filter conc-cont* (:content rec))]
    (let [newcontent (filter (complement conc-cont*) (:content rec))
	  value (apply str (map conc-cont* cont-recs))]
      (-> rec
	  (update-in [:value] str value)
	  (update-in [:content] (constantly newcontent))))))


(defn gedcom-reduce-content
  "Reduce a partition with records from a single stanza to an xml-like structure by adding
the sub-stanzas to the :content of the record"
  ([prt]
     (let [[level rec] (first prt)
	   rec (assoc rec :content (gedcom-reduce-content (rest  prt) []))]
       (-> rec conc-cont)))
       	

  ([prt vec]
     (if-let [prt (seq prt)]
       (let [[level rec] (first prt)
             subparts (partition-starting-every (level? level) prt)]
         (reduce conj [] (map gedcom-reduce-content subparts))))))

(defrecord GedcomAttrributes [level line-number representation])
(defrecord GedcomRecord [tag value attrs content])

(defn get-at 
  "Return a collection of GedcomRecord matching the tags.

  For example to get the given names of an individual use [:INDI :NAME :GIVN]"
  [rec tags]
  (letfn [(get* [recs [tag & tags]]
            (if tag
              (let [f (if (seq tags) (partial mapcat :content) (partial map identity))]
                (recur
                 (f (filter #(do (= tag (:tag %))) recs))
                 tags))
              recs)
            )]
    (get* [rec] tags)))

(defn get-value-at [rec tags]
  (map :value (get-at rec tags)))
           
  

(defn gedcom-line-to-record
  "Convert a line from a gedcom file into a vector with level and a map"
  [lineno line]
  (let [[level preid prerest]  (str/split line #" " 3)
        [id value] (if (re-matches #"@.*@" preid) [prerest preid] [preid prerest])
        attrmap {:level level :tag id}
	attrs (GedcomAttrributes. level lineno line) ]
    [level (GedcomRecord. (keyword (str/trim id)) value attrs nil)]))

(defn source-line "Get a vector with [level GedcomRecord] and return the source line"
  [[level rec]]  (-> rec :attrs :representation))


(defn gedcom-partitions  [inseq]
  "Partition the gedcom lines into stanzas"
  (let [recordseq (map-indexed #(gedcom-line-to-record (inc %1) %2) inseq)
        parts (partition-starting-every (level? "0") recordseq)]
    parts ))

(defn add-source-stanza [m]
  (assoc m
    :source-stanza-representation (reduce str
					  (interpose \newline
						     (map source-line m)))))

(defn lines-seq [s]
  (filter #(> (count %) 0) s))

(defmulti gedcom-seqx #(gedcom? %))
(defmethod gedcom-seqx "0 HEAD" [str]
	   (let [aseq (lines-seq str)]
	     (map #(-> % gedcom-reduce-content add-source-stanza)
		  (gedcom-partitions aseq))))
(defn gedcom-seq [aseq]
	   (map #(-> % gedcom-reduce-content add-source-stanza)
		(gedcom-partitions (lines-seq aseq))))

;  "Return a sequence of GEDCOM records following the pattern that the XML parsing uses."
;  [f]
;  (map # (gedcom-reduce-content %) 
;           (gedcom-partitions f)))


(defn parse
  "Parse a GEDCOM file and produce an structure similar to the xml parse."
  [f] (with-open [rdr (reader f)]
	(let [content (reduce conj [] (gedcom-seq rdr))]
	  { :tag :GEDCOM :content content})) )


(defn INDI [f]
  (let [c (-> f :content)
	name  (map #(-> % :attrs :value) (filter #(= (% :tag) :NAME) c))]
    name))

;;(take 3 (map #(apply-symbol (:tag %) (list %)) gseq))

;;
;; test stuff
;;
(comment
  (def file "src/test/resources/simple.ged")

  (def parts (gedcom-partitions file))

  (def xml (parse file)))


