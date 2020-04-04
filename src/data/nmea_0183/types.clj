(ns data.nmea-0183.types
  "Defines NMEA 0183 date types and their parsing from ASCII."
  (:require [clojure.spec.alpha :as s]))

;; Define data types
(s/def ::double double?)
(s/def ::string string?)
(s/def ::integer integer?)

(s/def ::faa-mode #{:automatic :manual :dgps :estimated :precise :simulated :none})
(s/def ::gps-fix-status #{:na :2d :3d})
(s/def ::gps-fix-quality #{:invalid :normal :dgps :pps :rtk :frtk :estimated :manual :simulated})

(s/def ::boolean boolean?)

(s/def ::hours integer?)
(s/def ::minutes integer?)
(s/def ::seconds double?)

(s/def ::utc-time (s/keys :req [::hours ::minutes ::seconds]))

(s/def ::date #(instance? java.time.LocalDate %))

;; Define a multi method to parse different field types from ascii

(defmulti from-ascii
  "Parse NMEA 0183 field from string to Clojure data. Dispatches on data type keyword."
  (fn [type ascii-value] type))

(defmethod from-ascii ::double [_ d]
  (Double/parseDouble d))

(defmethod from-ascii ::string [_ s] s)

(defmethod from-ascii ::boolean [_ b]
  (case b
    "T" true
    "F" false))

(defmethod from-ascii ::faa-mode [_ mode]
  (case mode
    "A" :automatic
    "M" :manual
    "D" :dgps
    "E" :estimated
    "P" :precise
    "S" :simulated
    "N" :none))

(defmethod from-ascii ::gps-fix-status [_ status]
  (case status
    "1" :na
    "2" :2d
    "3" :3d))

(defmethod from-ascii ::gps-fix-quality [_ quality]
  (case quality
    "0" :invalid
    "1" :normal
    "2" :dgps
    "3" :pps
    "4" :rtk
    "5" :frtk
    "6" :estimated
    "7" :manual
    "8" :simulated
    :unknown))

(defmethod from-ascii ::utc-time [_ value]
  {::hours (Integer/parseInt (subs value 0 2))
   ::minutes (Integer/parseInt (subs value 2 4))
   ::seconds (Double/parseDouble (subs value 4))})

(s/def ::latitude double?)

(defn- parse-degree [integer-part-len value]
  (let [deg (Integer/parseInt (subs value 0 integer-part-len))
        min (Double/parseDouble (subs value integer-part-len))]
    (+ deg (/ min 60))))

(defmethod from-ascii ::latitude [_ value]
  (parse-degree 2 value))

(s/def ::longitude double?)

(defmethod from-ascii ::longitude [_ value]
  (parse-degree 3 value))

(s/def ::hemisphere #{:north :south :west :east})

(defmethod from-ascii ::hemisphere [_ value]
  (case value
    "N" :north
    "S" :south
    "W" :west
    "E" :east))

(defmethod from-ascii ::integer [_ value]
  (Integer/parseInt value))

(s/def ::units #{:meters :feet})

(defmethod from-ascii ::units [_ value]
  (case value
    "F" :feet
    "M" :meters))

(s/def ::data-status #{:active :void})

(defmethod from-ascii ::data-status [_ value]
  (case value
    "A" :active
    "V" :void))

(defmethod from-ascii ::date [_ value]
  (let [dd (Integer/parseInt (subs value 0 2))
        mm (Integer/parseInt (subs value 2 4))
        yy (Integer/parseInt (subs value 4))]
    (java.time.LocalDate/of (+ 2000 yy) mm dd)))
