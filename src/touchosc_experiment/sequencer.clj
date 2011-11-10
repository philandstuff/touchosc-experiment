(ns touchosc-experiment.sequencer
  (:use [touchosc-experiment.touchosc :only (simple-handler register-handlers)])
  (:use overtone.core))

(defonce sequencer-state (atom (vec (repeat 8 (vec (repeat 8 0.0))))))

(definst kick []
  (let [src (+ (sin-osc 80) (sin-osc 160))
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 3.5 src env)))

(definst hat []
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

(defonce beep-freq (atom 1100))

(definst beep [freq 1100]
  (let [src (sin-osc freq)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 2 src env)))

(defonce dubstep-note (atom 25))
(defonce sweep-rate (atom 4))

(definst dubstep [note 25 sweep-rate 4 vol 0 hi? 0 sweep? 0 decimate? 0]
  (let [trig (coin-gate 0.5 (impulse:kr 2))
        freq (midicps note)
        sweep (lin-exp (lf-saw sweep-rate) -1 1 40 5000)
        son (->
             (mix (lf-saw (* freq [0.99 1 1.01])))
             (lpf sweep)
             (normalizer))
        son (+ son (bpf son 2000 2))
        son (select hi? [son (* 4 (hpf son 1000))])
        son (select sweep? [son (* 4 (hpf son sweep))])
        son (select decimate? [son (round son 0.1)])
        son (tanh (* son 5))
        son (+ son (* 0.3 (g-verb son 10 0.1 0.7)))]
    (* vol son)))

(dubstep)

(def metro (metronome 128))

(def inst-map
  {0 kick
   1 hat
   2 #(beep @beep-freq)
   3 dubstep})

(defn sequencer [beat notes]
  (at (metro beat)
      (let [mod-beat (mod beat 8)]
        (doseq [[index inst] inst-map]
          (let [val (get-in @notes [index mod-beat])]
            (if (= 3 index)
              (ctl dubstep :vol val)
              (when (not (zero? val))
                (inst)))))))
  (apply-at (metro (inc beat)) #'sequencer (inc beat) [notes]))

(defn sequencer-off []
  (stop)
  )

(defn sequencer-on []
  (sequencer (metro) sequencer-state))

(defn make-sequencer-handler [inst beat]
  (simple-handler [arg]
                  (swap! sequencer-state
                         assoc-in [inst beat] arg)))

(def ^{:private true} multitoggle-map-entries
  (let [path-from (fn [x y] (str "/4/multitoggle/" (inc y) "/" (inc x)))]
    (for [x (range 8)
          y (range 8)]
      [(path-from x y) (make-sequencer-handler x y)])))

(def sequencer-map
  (into
   {"/4/toggle1" (simple-handler [arg] (if (zero? arg) (sequencer-off) (sequencer-on)))
    "/1/fader1"  (simple-handler [arg] (reset! beep-freq (scale-range arg 0 1 250 1500)))
    "/1/fader2"  (simple-handler [arg] (ctl dubstep :note (round-to (scale-range arg 0 1 20 50) 1)))
    "/1/fader3"  (simple-handler [arg] (ctl dubstep :sweep-rate (scale-range arg 0 1 1 16)))
    "/1/toggle2" (simple-handler [arg] (ctl dubstep :hi? arg))
    "/1/toggle3" (simple-handler [arg] (ctl dubstep :sweep? arg))
    "/1/toggle4" (simple-handler [arg] (ctl dubstep :decimate? arg))
    "/1/fader5"  (simple-handler [arg]
                                 (let [new-bpm (scale-range arg 0 1 160 500)]
                                   (metro :bpm new-bpm)))}
   multitoggle-map-entries))

(register-handlers sequencer-map)