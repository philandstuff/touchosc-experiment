(ns touchosc-experiment.insts
  (:use overtone.core))

;; need to connect to a server for the definsts to make sense
(when-not (server-connected?)
  (boot-external-server))

(definst kick [volume 1.0]
  (let [body-freq (* 220 (env-gen (lin-env 0.01 0 0.3 1) :action NO-ACTION))
        body (sin-osc body-freq)
        
        pop-freq (+ 20 (* 400 (env-gen (lin-env 0.01 0 0.1 1) :action NO-ACTION)))
        pop  (sin-osc pop-freq)
        
        env  (env-gen (perc 0.001 1.5) :action FREE)
        ]
    (* env (+ body pop))))

(definst hat []
  (let [src (white-noise)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

(definst bass [freq 50 volume 1.0 wah 1.0]
  (let [son (saw freq)
        son (* son wah)
        son (clip:ar son 0 1)]
    (* volume son)))

(definst beep [freq 1100]
  (let [src (sin-osc freq)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 2 src env)))

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

