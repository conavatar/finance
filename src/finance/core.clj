(ns finance.core
  (:use [clojure.contrib.http.agent :only [http-agent status string]])
;;  (:require [clojure.contrib.string :as ss])
  (:require [clj-time.core :as time])
  )

;; In the :doc line if i split it to next line it doesn' read it correctly
;; gives error in the swank mode...haven't tried out compiling

;; will need to test out incanter to view it...
;; need to remove the image stuff

;; https://github.com/ghoseb/yfinance/blob/master/src/in/freegeek/yfinance.clj below code from 

(def #^{:private true} +base-url+ "http://itable.finance.yahoo.com/table.csv?s=%s&g=d&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d")

(defn- get-full-url
  "Construct the complete URL given the params"
  [y1 m1 d1 y2 m2 d2 sym]
  (let [start (time/date-time y1 m1 d1)
        end (time/date-time y2 m2 d2)]
    (format +base-url+
            sym
            (dec (time/month start))
            (time/day start)
            (time/year start)
            (dec (time/month end))
            (time/day end)
            (time/year end))))


(defn- fetch-url
  "Fetch one URL using HTTP Agent"
  [url]
  (http-agent url))

;; string method of http.agent prints the output correctly, as i had removed it, it was causing issue in getting the data
(defn- collect-response
  "Wait for all the agents to finish and then return the response"
  [& agnts]
  (apply await agnts)                   ; FIXME: Have a sane timeout
  (for [a agnts]
    (if (= (status a) 200)
      (string a)
      (status a))))



(defn fetch-historical-data
  "Fetch historical prices from Yahoo! finance for the given symbols between start and end"
  [start end syms]
  (letfn [(parse-date [dt] (map #(Integer/parseInt %) (.split dt "-")))]
    (let [[y1 m1 d1] (parse-date start)
          [y2 m2 d2] (parse-date end)
          urls (map (partial get-full-url y1 m1 d1 y2 m2 d2) syms)
          agnts (map fetch-url urls)]
      (zipmap syms (apply collect-response agnts)))))



;;;; -- my functions --- ;;;;
(defrecord stock [name info])

(defn rest-stock-info [data]
  (let [info (vals data)]
    (rest (clojure.contrib.string/split-lines (first info)))))

(defn parse-date [date]
  (let [[y m d] (map #(Integer/parseInt %) (.split date "-"))]
    (time/date-time y m d)))


(defn parse-stock-info [str-arr]
  (let [ coll (.split str-arr ",")]
    `[~(parse-date (first coll)) ~@(map #(Float/parseFloat %) (rest coll))]
    ))

(defn create-stock [data]
  (let [name (keys data)
        info (rest-stock-info data)]
    (stock. name (map #(parse-stock-info %) info))
    ))