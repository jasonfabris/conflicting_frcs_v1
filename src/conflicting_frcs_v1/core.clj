(ns conflicting_frcs_v1.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [conflicting_frcs_v1.pacer :as p]))

;;[conflicting_frcs_v1.math.vector :as mv]
(def my-seed 10)
(def num-pacers 100)

(def normals
  (let [rgen (java.util.Random. my-seed)]
    (repeatedly #(-> rgen .nextGaussian (* 1)))))

(defn update-force [f]
  (let [[x y] f
        x-el (+ 5 (rand-int 5000))
        y-el (+ 5 (rand-int 5000))
        x-n (* 20 (nth normals (q/frame-count)))
        y-n (* 20 (nth normals (q/frame-count)))]
    ;;(println "updating:" x-n " : " y-n)
    [(+ x x-n) 
     (+ y y-n)]))


(def tstps (mapv p/new-pacer 
                 [[200 200] [1000 10] [10 1000] [1000 1000]]))

(defn replace-ob-pacers [pacers]
  (let [cnt (count pacers)
        retained (vec (remove p/check-edges pacers))
        new-p (fn [] 
                (p/new-pacer [(* (q/width) (Math/random)) 
                              (* (q/height) (Math/random))]))]
    (into retained (repeatedly (- cnt (count retained)) new-p)))
  )
        ;;(p/new-pacer (/ (q/width) 2) (/ (q/height) 2))))))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 5)
  (q/color-mode :hsb 360 100 100 1)
  (q/background 202 30 40)
  
  (let [rng 500 ;;(/ (q/width 3) 
        lb (/ (- (q/width) rng) 2)
        ub (+ lb rng)
        origins (for [v (repeat num-pacers [])]
                  (into v (conj [(+ lb (* rng (Math/random))) 
                                 (+ lb (* rng (Math/random)))])))
        ps (mapv p/new-pacer origins)
        frc1 (vec (map #(* 10 %) (vec (take 2 normals))))
        frc2 (vec (map #(* 30 %) (vec (take 2 normals))))]
    {:p1 (p/new-pacer [(/ (q/width) 2) (/ (q/height) 2)])
     :pacers ps
     :forces (conj [] frc1 frc2)}))

(defn update-state [state]  
  {:forces (map update-force (:forces state))
   ;;:pacers (map #(p/update-loc %1 (:forces state)) (:pacers state))
   ;;:p1 (p/update-pacer (:p1 state))
    :pacers (->> 
             (map #(p/update-loc %1 (:forces state)) (:pacers state))
             (replace-ob-pacers))
   }
  )

(defn draw-state [state]
  ;;(q/no-loop)
  (q/print-first-n 5 (str ":::Frame: " (q/frame-count) " " 
                          (:p1 state) " " (count (:pacers state))))
  ;;(q/print-first-n 25 (str (first (:forces state))))
  (q/fill 202 25 200)
  (q/stroke-weight 1)
  (q/stroke 200 50 90 0.5)
  
  (println "Force: " (:forces state))
  (doseq [p (:pacers state)]
    (p/display-pacer p)
    ;;(println p) ;; (:location p))
    ))


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




