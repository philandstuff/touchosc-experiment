(ns touchosc-experiment.touchosc
  (:use overtone.core)
  (:use overtone.inst.piano))

(defonce server (atom nil))

(defn start-osc []
  (swap! server (fn [old-server]
                  (if (not (nil? old-server))
                    old-server
                    (osc-server 44100 "Phil's overtone")))))

(defn register-debug-listener []
  (osc-listen @server (fn [msg] (println msg)) :debug))

(defn remove-debug-listener []
  (osc-rm-listener @server :debug))

(defmacro simple-handler [args body]
  `(fn [msg#]
     (let [~args (:args msg#)]
       ~body)))

(defn make-piano-handler [note]
  (simple-handler [arg]
                  (when (not (zero? arg))
                    (piano note))))

(def piano-map
  (let [middle-b 59
        indices (range 1 13)
        path-from (fn [index] (str "/1/push" index))
        note-from (fn [index] (+ middle-b index))]
    (zipmap
     (map path-from indices) (map (comp make-piano-handler note-from) indices))))

(defn register-handlers [handler-map]
  (doseq [[path handler] handler-map]
    (osc-handle @server path handler))
  @server)

