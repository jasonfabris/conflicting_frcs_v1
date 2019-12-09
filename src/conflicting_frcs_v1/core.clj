(ns conflicting_frcs_v1.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [conflicting_frcs_v1.pacer :as p]
            [conflicting_frcs_v1.math.vector :as mv]))


(def normals
  (let [rgen (java.util.Random. 20)]
    (repeatedly #(-> rgen .nextGaussian (* 0.5)))))

(defn update-forces [forces]
  forces)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 5)
  (q/background 24 0 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  ;(q/color-mode :hsb)
  (let [ps (into [] (repeat 5
            (p/new-pacer [(/ (q/width) 2) (/ (q/height) 2)])))
        frc1 (vec (map #(* 10 %) (vec (take 2 normals))))]
    {:color 0
     :angle 0
     :fill 0
     :p1 (p/new-pacer [(/ (q/width) 2) (/ (q/height) 2)])
     :pacers ps
     :forces (into [] frc1)}))

(defn update-state [state]
  
    {:color (mod (+ (:color state) 2) 255)
     :fill (mod (+ (:fill state) 5) 255)
     :forces (update-forces (:forces state))
     :p1 (p/update-pacer (:p1 state))
     :pacers (map #(p/update-loc %1 (:forces state)) (:pacers state))})

(defn draw-state [state]
  (q/no-loop)
  (q/print-first-n 3 (str ":::Frame: " (q/frame-count) " " 
                          (:p1 state) " " (count (:pacers state))))
  (q/print-first-n 3 (str (:forces state)))
  (q/fill (:fill state) 25 (* (rand) 255))
  (q/color (:color state) 255 (* (rand) 255))
  ;;(p/display-pacer (:p1 state))
  (doseq [p (:pacers state)]
    (let [[x y] (:location p)]
      (println (q/frame-count))
      (q/ellipse x y 20 20))))

(q/defsketch conflicting_frcs_v1
  :title "Conflict"
  :size [1200 1200]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])


; (let [[x y]
; (:location (-> (p/new-pacer [210 100])
;                (p/find-target ,,, 50)
;                (p/apply-force ,,, [(* (rand) 500)  (* (rand) 20)])
;                (p/update-loc)))]
; (str [x y]))

