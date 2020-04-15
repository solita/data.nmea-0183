(ns data.nmea-0183.sentences
  "Define NMEA 0183 sentences"
  (:require [clojure.spec.alpha :as s]
            [data.nmea-0183.fields :as f]
            [data.nmea-0183.types :as t]
            [clojure.string :as str]))

(defn- parse [field-values & field-keywords]
  (loop [result {}
         [value & values] field-values
         [kw & keywords] field-keywords]
    (if-not kw
      result
      (if (vector? kw)
        ;; Vector definition of [count field-type-keyword collection-keyword validator] that specifies a collection
        (let [[cnt coll-kw validator] kw
              ;; If we are dealing with a dynamic length collection, assume that a collection length is defined
              ;; before the collection values and that the cnt is a keyword that points to the length value.
              cnt (if (keyword? cnt) (cnt result) cnt)]
          ;; If coll-key is a map, we take key and value pairs from the processed fields.
          (if (= (first (s/describe coll-kw)) 'map-of)
            (let [[_ key-kw val-kw] (s/describe coll-kw)

                  new-key (f/parse-field key-kw value)
                  new-value (f/parse-field val-kw (first values))
                  result (if (or (nil? validator) (validator new-key))
                           (update result coll-kw assoc new-key new-value)
                           result)]
              (if (= 1 cnt)
                ;; Last one parsed
                (recur result (rest values) keywords)
                ;; Still more to parse
                (recur result (rest values) (cons [(dec cnt) coll-kw validator] keywords))))
            ;; If coll-key is a sequence
            (let [kw (second (s/describe coll-kw))
                  new-value (f/parse-field kw value)
                  result (if (or (nil? validator)
                                 (validator new-value))
                           (update result coll-kw (fnil conj []) new-value)
                           result)]
              (if (= 1 cnt)
                ;; Last one parsed
                (recur result values keywords)
                ;; Still more to parse
                (recur result values (cons [(dec cnt) coll-kw validator] keywords))))))

        ;; Regular single values field
        (recur (assoc result kw (f/parse-field kw value))
               values
               keywords)))))

(defmacro define-sentence [id kw & field-parse-defs]
  `(do
     (s/def ~kw (s/keys :req [~@(for [f field-parse-defs]
                                  (if (vector? f)
                                    (nth f 2)
                                    f))]))
     (defmethod parse-sentence ~id [_# values#]
       {::type ~kw
        ~kw (parse values# ~@field-parse-defs)})))

(defmulti parse-sentence
  "Parse sentence by sentence id keyword. Takes the sentence id and collection of field values."
  (fn [sentence-kw field-values] sentence-kw))

(define-sentence "GSA" ::gsa
  ::f/faa-mode
  ::f/fix-status
  [12 ::f/satellite-ids (complement str/blank?)]
  ::f/position-dop
  ::f/horizontal-dop
  ::f/vertical-dop)

(define-sentence "HDT" ::hdt
  ::f/heading ::f/true-indicator)

(define-sentence "VTG" ::vtg
  ::f/true-course ::f/true-indicator
  ::f/magnetic-course ::f/magnetic-indicator
  ::f/speed-knots ::f/knots-indicator
  ::f/speed-kmph ::f/kmph-indicator
  ::f/faa-mode)

(define-sentence "GGA" ::gga
  ::f/time ;; <- UTC of position fix
  ::f/latitude ::f/lat-hemisphere ::f/longitude ::f/lon-hemisphere
  ::f/fix-quality
  ::f/satellites-in-use
  ::f/horizontal-dilution
  ::f/altitude ::f/altitude-units
  ::f/geoidal-height ::f/height-units
  ::f/dgps-age ::f/dgps-station-id)

(define-sentence "ROT" ::rot
  ::f/rate-of-turn
  ::f/data-status)

(define-sentence "RRE" ::rre
  ::f/satellites-in-use
  ;; Using kw as the first value, the parser that the collection length is dynamic and the cnt value should be fetched
  ;; from the specified kw.
  [::f/satellites-in-use ::f/range-residuals (complement str/blank?)]
  ::f/hpos-err-estimate
  ::f/vpos-err-estimate)

(define-sentence "ZDA" ::zda
  ::f/time ::f/day ::f/month ::f/year
  ::f/local-zone-hours ::f/local-zone-minutes)

(define-sentence "RMC" ::rmc
  ::f/time ;; <-- UTC of position fix
  ::f/data-status ;; <-- Status A=active or V=void
  ::f/latitude ::f/lat-hemisphere
  ::f/longitude ::f/lon-hemisphere
  ::f/speed-knots
  ::f/course
  ::f/date
  ::f/magnetic-variation
  ::f/variation-hemisphere)

(define-sentence "GRS" ::grs
  ::f/time ;; <-- UTC of GGA position fix
  ;; 	Residuals recomputed
  ;;   0: Residuals used to calculate position given in the matching GGA line
  ;;   1: Residuals recomputed after the GGA position was computed
  ::f/residuals-recomputed
  [12 ::f/grs-range-residuals some?])

;; GSV - satellites in view
;; GNS - ?
;; GSA - GPS DOP and active satellites
;; GGA - GPS fix data. Time, position and fix related data for a gps receiver
;; HDT - Heading true
;; RMC - Recommended minimum navigation information
;; ROT - rate of turn
;; VTG - Track Made Good and Ground Speed
;; ZDA - Time and date
