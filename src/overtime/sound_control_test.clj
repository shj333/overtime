(ns overtime.sound-control-test
  (:require [overtone.core :as ot]
            [overtime.microsound :as micro]
            [overtime.sound-control :as snd]))

(defn def-synths
  []
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
                 (ot/out:ar out (* amp (ot/grain-sin:ar 2 trigger grain-dur this-freq pan env-buf))))))

(def sound-data {:busses       {:fx      {:reverb-bus [:audio 1]}
                                :control {:swoop-bus [:control 7]}}
                 :micro        {:triggers      {:coin2 micro/coin-trigger}
                                :density-range [2 10]}
                 :def-synths-f def-synths
                 :sounds       {:reverb1    {:instr  reverb1
                                             :params {:group       :fx
                                                      :in          :reverb-bus
                                                      :room-size   243
                                                      :rev-time    1
                                                      :damping     0.1
                                                      :input-bw    0.34
                                                      :dry-level   -3
                                                      :early-level -11
                                                      :tail-level  -9}}
                                :high-bells {:synth  my-grain-sin
                                             :params {:env-buf        :expodec
                                                      :trigger-bus    :rand-sync
                                                      :pan-bus        :rand-sync
                                                      :grain-dur      2.0
                                                      :freq           2000
                                                      :freq-dev-noise 1000
                                                      :amp            0.1
                                                      :out            :reverb-bus}}
                                :low-rumble {:synth  my-grain-sin
                                             :params {:env-buf        :guass
                                                      :trigger-bus    :async
                                                      :pan-bus        :async
                                                      :grain-dur      2.0
                                                      :freq           100
                                                      :freq-dev-noise 10
                                                      :amp            0.4}}}})
(comment
  (if (ot/server-disconnected?) (ot/connect-external-server 4445))
  (snd/define-sounds sound-data))
