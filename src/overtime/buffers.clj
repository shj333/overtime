(ns overtime.buffers
  (:require [overtone.core :as ot]
            [incanter core charts datasets]
            [overtime.sounds :as snd]
            [overtime.utils :as u]
            [clojure.tools.logging :as log]))


(defonce ^:private buffers (atom {}))


;
; Public API
;
(defn buffer-categories [] (keys @buffers))

(defn buffer-keys
  [category]
  (-> (u/check-nil (category @buffers) "Buffer Category" category)
      keys
      sort))

(defn buffer
  "Returns the buffer with the given category and key"
  [category key]
  (u/check-nil (get-in @buffers [category key]) "Buffer" category key))

(defn signals->buffer
  [signals]
  (let [b (ot/buffer (count signals))]
    (ot/buffer-write! b signals)
    b))

(defn add-buffers
  [category new-buffers]
  (swap! buffers assoc category new-buffers))

(defn view-buffer
  "View buffer as X/Y plot using Incanter view"
  [category key]
  (let [buf (buffer category key)
        buf-vals (into '() (ot/buffer-read buf))]
    (-> (ot/num-frames buf)
        range
        (incanter.charts/xy-plot buf-vals :title (str (name category) " buffer: " (name key)) :x-label "range" :y-label "buffer vals")
        incanter.core/view)))

(defn view-category-buffers
  "View buffer as X/Y plot using Incanter view"
  [category]
  (doseq [key (buffer-keys category)] (view-buffer category key)))

(defn- init-buffers
  [category buffers-map]
  (log/debug "Creating buffers for category" category "=>" (keys buffers-map))
  (into {} (for [[key gen-signals-f] buffers-map] [key (signals->buffer (gen-signals-f))])))

(defn init
  ""
  [categories-map]
  (->> (into {} (for [[category buffers-map] categories-map] [category (init-buffers category buffers-map)]))
       (reset! buffers))
  (log/info "Finished buffers init")
  (doseq [category (buffer-categories)] (log/info "Buffer category" category ":" (buffer-keys category)))
  true)


(defmethod snd/sound-param-val :audio-buf [_type val] (buffer :audio val))
(defmethod snd/sound-param-val :env-buf [_type val] (buffer :env val))
(defmethod snd/sound-param-val :wave-buf [_type val] (buffer :wave val))
