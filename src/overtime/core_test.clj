(ns overtime.core-test
  (:require [overtone.core :as ot]
            [overtime.core :as otm]
            [overtime.instruments :as instr]
            [overtime.microsounds :as micro]
            [overtime.patterns :as pat]))

(defn def-synths
  []
  (ot/defsynth band-pass [in 0 out 0 center-freq 200 rq 0.3]
               (let [sig (ot/in:ar in)]
                 (ot/out:ar out (ot/bpf:ar sig center-freq rq))))

  (ot/defsynth reverb1
               [in 0 out 0 room-size 10 rev-time 3 damping 0.5 input-bw 0.5 spread 15 dry-level 1 early-level 0.7 tail-level 0.5 room-size 300]
               (let [sig (ot/in:ar in)
                     dry-level-amp (ot/dbamp dry-level)
                     early-level-amp (ot/dbamp early-level)
                     tail-level-amp (ot/dbamp tail-level)]
                 (ot/out:ar out (ot/g-verb:ar sig room-size rev-time damping input-bw spread dry-level-amp early-level-amp tail-level-amp room-size))))

  (ot/defsynth my-grain-sin [out 0 env-buf -1 trigger-bus 0 grain-dur 0.1 freq 440 freq-dev-noise 400 amp 0.05 pan-bus 0]
               (let [trigger (ot/in:kr trigger-bus 1)
                     pan (ot/in:kr pan-bus 1)
                     freq-dev (* (ot/white-noise:kr) freq-dev-noise)
                     this-freq (+ freq freq-dev)]
                 (ot/out:ar out (* amp (ot/grain-sin:ar 2 trigger grain-dur this-freq pan env-buf)))))

  (ot/defsynth gabor [out [0 :ir] freq [440 :ir] sustain [1 :ir] pan [0.0 :ir] amp [0.1 :ir] width [0.25 :ir]]
               (let [env (ot/lf-gauss:ar sustain width :loop 0 :action ot/FREE)
                     half-pi (* 0.5 (. Math PI))
                     son (* env (ot/f-sin-osc:ar freq half-pi))]
                 (ot/offset-out:ar out (ot/pan2:ar son pan amp)))))


(def sound-data {:busses       {:fx      {:band-pass-bus [:audio 1]
                                          :reverb-bus    [:audio 1]}
                                :control {:swoop-bus [:control 7]}}
                 :micro        {:triggers      {:coin2 #'micro/coin-trigger}
                                :density-range [2 10]}
                 :def-synths-f #'def-synths
                 :sounds       {:band-pass  {:synth     #'band-pass
                                             :group     :fx
                                             :group-pos :head
                                             :params    {:in          :band-pass-bus
                                                         :out         :reverb-bus
                                                         :center-freq 440
                                                         :rq          1.0}}
                                :reverb1    {:synth     #'reverb1
                                             :group     :fx
                                             :group-pos :tail
                                             :params    {:in          :reverb-bus
                                                         :room-size   243
                                                         :rev-time    1
                                                         :damping     0.1
                                                         :input-bw    0.34
                                                         :dry-level   -3
                                                         :early-level -11
                                                         :tail-level  -9}}
                                :high-bells {:synth  #'my-grain-sin
                                             :params {:env-buf        :expodec
                                                      :trigger-bus    :rand-sync
                                                      :pan-bus        :rand-sync
                                                      :grain-dur      2.0
                                                      :freq           2000
                                                      :freq-dev-noise 1000
                                                      :amp            0.1
                                                      :out            :reverb-bus}}
                                :low-rumble {:synth  #'my-grain-sin
                                             :params {:env-buf        :guass
                                                      :trigger-bus    :async
                                                      :pan-bus        :async
                                                      :grain-dur      2.0
                                                      :freq           100
                                                      :freq-dev-noise 10
                                                      :amp            0.4}}}
                 :patterns     {:gabor {:synth  #'gabor
                                        :params {:out            0
                                                 :freq           1000
                                                 :sustain        0.02
                                                 :pan            0.0
                                                 :amp            0.2
                                                 :dur            500
                                                 :staging-period 1000}}}})

(def sections-data [{:name   "Section 1"
                     :length 17
                     :events [[0.0 :play :reverb1]
                              [0.0 :play :high-bells]
                              [3.3 :play :low-rumble]
                              [10.0 :stop :low-rumble]
                              [10.0 :set :instr :high-bells :amp 0.1]
                              [12.0 :set :instr :high-bells :amp 0.01]
                              [15.0 :stop :high-bells]]}
                    {:name   "Section 2"
                     :length 10
                     :events [[0.0 :play :gabor]
                              [5.0 :set :pat :gabor :sustain (map #(/ % (pat/current-value :gabor :freq)) (range 10 0 -1))]]}
                    {:name   "Done"
                     :length 0
                     :events []}])

(comment
  (otm/init sound-data)
  (otm/play-sections sections-data)
  (pat/play-at (ot/now) :gabor)
  (pat/stop-at (ot/now) :gabor)
  (pat/reset-pattern! :gabor)
  (instr/set-params-at (ot/now) :pat :gabor :sustain 0.02)
  (ot/sc-osc-debug-off))
