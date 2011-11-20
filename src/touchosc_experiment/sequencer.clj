(ns touchosc-experiment.sequencer
  (:use [touchosc-experiment.touchosc :only (simple-handler register-handlers)])
  (:require [touchosc-experiment [insts :as inst]])
  (:use overtone.core))

(defonce sequencer-state (atom (vec (repeat 8 (vec (repeat 8 0))))))

(defonce beep-freq (atom 1100))

(defonce dubstep-note (atom 25))
(defonce sweep-rate (atom 4))
(defonce sequencer-on? (atom false))

(def metro (metronome 256))

(def inst-map
  {0 {1 inst/kick}
   1 {1 inst/hat}
   2 {1 #(inst/beep @beep-freq)}
   3 inst/dubstep
   4 {1 #(ctl inst/bass :volume 1) 0 #(ctl inst/bass :volume 0)}})

(defn sequencer [beat notes]
  (when @sequencer-on?
    (at (metro beat)
        (let [mod-beat (mod beat 8)]
          (doseq [[index inst] inst-map]
            (let [val  (get-in @notes [index mod-beat])
                  inst (get inst val (fn [] nil))]
              (inst)))))
    (apply-at (metro (inc beat)) #'sequencer (inc beat) [notes])))

(defn sequencer-off []
  (reset! sequencer-on? false)
  (kill inst/bass)
  (kill inst/dubstep)
  )

(defn sequencer-on []
  (inst/bass :volume 0)
  (reset! sequencer-on? true)
  (sequencer (metro) sequencer-state))

(defn make-sequencer-handler [inst beat]
  (simple-handler [arg]
                  (swap! sequencer-state
                         assoc-in [inst beat] (int arg))))

(def ^{:private true} multitoggle-map-entries
  (let [path-from (fn [x y] (str "/4/multitoggle/" (inc y) "/" (inc x)))]
    (for [x (range 8)
          y (range 8)]
      [(path-from x y) (make-sequencer-handler x y)])))

(def sequencer-map
  (into
   {"/4/toggle1" (simple-handler [arg] (if (zero? arg) (sequencer-off) (sequencer-on)))
    "/1/fader1"  (simple-handler [arg] (reset! beep-freq (scale-range arg 0 1 250 1500)))
    "/1/fader2"  (simple-handler [arg] (ctl inst/dubstep :note (round-to (scale-range arg 0 1 20 50) 1)))
    "/1/fader3"  (simple-handler [arg] (ctl inst/dubstep :sweep-rate (scale-range arg 0 1 1 16)))
    "/1/toggle2" (simple-handler [arg] (ctl inst/dubstep :hi? arg))
    "/1/toggle3" (simple-handler [arg] (ctl inst/dubstep :sweep? arg))
    "/1/toggle4" (simple-handler [arg] (ctl inst/dubstep :decimate? arg))
    "/1/fader5"  (simple-handler [arg]
                                 (let [new-bpm (scale-range arg 0 1 160 500)]
                                   (metro :bpm new-bpm)))
    "/3/xy"      (simple-handler [arg1 arg2]
                                 (do
                                   (ctl inst/bass :freq (scale-range arg1 0 1 40 250))
                                   (ctl inst/bass :wah  (scale-range arg2 0 1 1.5 30))))}
   multitoggle-map-entries))

(touchosc-experiment.touchosc/start-osc)
(register-handlers sequencer-map)