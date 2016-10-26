(ns overtime.instr-gui
  (:require [seesaw.core :as ss]
            [seesaw.mig :as mig]
            [overtime.instruments :as instr]
            [clojure.tools.logging :as log]))

(ss/native!)

(defonce ^:private frames-data (atom {}))
(defonce ^:private slider-val-mult-dflt 1.0)

(defn- frame-data
  [frame-key]
  (let [data (frame-key @frames-data)]
    (if (nil? data)
      (throw (Exception. (str "Unknown frame:" frame-key)))
      data)))

(defn- update-instr
  [frame-key key val]
  (let [{:keys [instr-type instr]} (frame-data frame-key)]
    (log/debug "Frame" frame-key "has event" key "=>" val "for" (or instr-type "") (or instr "unknown"))
    (if-not (or (nil? instr-type) (nil? instr)) (instr/set-params instr-type instr key val))))

(defn- listbox-id [key is-lkup] (keyword (str (if is-lkup "#") (name key) "-lb")))

(defn- make-listbox-row
  [frame-key [listbox-key items]]
  (let [key-str (name listbox-key)
        listbox (ss/listbox :id (listbox-id listbox-key false) :model items)]
    (ss/listen listbox :selection (fn [_e] (update-instr frame-key listbox-key (ss/selection listbox))))
    [[key-str ""]
     [(ss/scrollable listbox) "wrap, gapbottom 10"]]))


(defn- slider-id [key is-lkup] (keyword (str (if is-lkup "#") (name key) "-sldr")))

(defn- make-slider
  [key min max val slider-val-mult]
  (ss/slider :id (slider-id key false) :value (* val slider-val-mult) :min (* min slider-val-mult) :max (* max slider-val-mult)))


(defn- text-box-id [key is-lkup] (keyword (str (if is-lkup "#") (name key) "-txt")))

(defn- make-text-box
  [key val]
  (ss/text :id (text-box-id key false) :text (str val)))


(defn- on-slider-event
  [slider textbox slider-val-mult]
  (ss/config! textbox
              :text
              (-> (ss/config slider :value)
                  (/ slider-val-mult))))

(defn- on-textbox-event
  [textbox slider slider-val-mult]
  (->> (ss/config textbox :text)
       Double.
       (* slider-val-mult)
       (ss/config! slider :value)))

(defn- make-slider-row
  [frame-key [slider-key {:keys [min max init-val slider-val-mult] :or {slider-val-mult slider-val-mult-dflt}}]]
  (let [key-str (name slider-key)
        slider (make-slider slider-key min max init-val slider-val-mult)
        textbox (make-text-box slider-key init-val)]
    (ss/listen slider #{:key-released :mouse-dragged} (fn [_e] (on-slider-event slider textbox slider-val-mult)))
    (ss/listen textbox :key-released (fn [_e] (on-textbox-event textbox slider slider-val-mult)))
    (ss/listen slider :state-changed (fn [_e] (update-instr frame-key slider-key (/ (ss/config slider :value) slider-val-mult))))
    [[key-str ""]
     [slider ""]
     [textbox "width 100:100:100, wrap"]]))

(defn- make-panel
  [frame-key listboxes sliders]
  (let [listbox-rows (mapcat make-listbox-row (repeat frame-key) listboxes)
        slider-rows (mapcat make-slider-row (repeat frame-key) sliders)]
    (mig/mig-panel :constraints ["" "" ""]
                   :items (concat listbox-rows slider-rows))))

(defn show
  [frame-key & {:keys [title listboxes sliders instr-type instr loc-x loc-y] :or {title "Instr GUI" listboxes [] sliders [] loc-x 0 loc-y 0}}]
  (let [f (ss/frame :title title :content "Placeholder...")
        p (make-panel frame-key listboxes sliders)]
    (log/debug "Showing GUI for" frame-key ", list boxes:" listboxes ", sliders:" sliders ", instr:" instr-type "-" instr)
    (ss/config! f :content p)
    (-> f ss/pack! ss/show!)
    (doto f (.setLocation loc-x loc-y))
    (swap! frames-data assoc frame-key {:frame f :sliders sliders :instr-type instr-type :instr instr})
    f))

(defn- widget
  [frame-key widget-id-f widget-key]
  (-> (frame-data frame-key)
      :frame
      (ss/select [(widget-id-f widget-key true)])))

(defn get-listbox [frame-key listbox-key] (widget frame-key listbox-id listbox-key))

(defn get-slider [frame-key slider-key] (widget frame-key slider-id slider-key))


(defn change-slider-val
  [frame-key slider-key val]
  (let [frame-data (frame-data frame-key)
        f (:frame frame-data)
        slider-val-mult (-> (:sliders frame-data)
                            slider-key
                            (get :slider-val-mult slider-val-mult-dflt))]
    (log/debug "Changing slider value for" frame-key slider-key "to" val)
    (-> (ss/select f [(slider-id slider-key true)])
        (ss/config! :value (* val slider-val-mult)))
    (-> (ss/select f [(text-box-id slider-key true)])
        (ss/config! :text (str val)))
    true))

(defn set-instr
  [frame-key instr-type instr]
  (log/debug "Set instr for" frame-key "to" instr-type instr)
  (swap! frames-data update frame-key assoc :instr-type instr-type :instr instr))

(comment
  (def listboxes {:grain-envs [:guass :expodec :sinc1 :sinc2 :sinc3 :sinc4 :sinc5 :sinc6 :sinc7 :sinc8 :sinc9 :sinc10]
                  :pans       [:left :right :center]})
  (def sliders {:grain-dur {:min 0.05 :max 2.0 :init-val 0.05 :slider-val-mult 100.0}
                :freq      {:min 10 :max 20000 :init-val 100}})
  (show key
            :title "Test GUI"
            :listboxes listboxes
            :sliders sliders
            :loc-x 2600
            :loc-y 200
            :instr-type :instr
            :instr :foo))
