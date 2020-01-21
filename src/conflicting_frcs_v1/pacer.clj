(ns conflicting_frcs_v1.pacer
  (:require [conflicting_frcs_v1.math.vector :as mv]
            [quil.core :as q]))

(defn new-pacer [loc]
  ;make a new pacer
     {:location loc
      :velocity [0 0]
      :acceleration [0 0]
      :mass (rand 120)
      :radius 100.0
      :target (mv/add loc [(rand-int 20) (rand-int 20)])
      :max-speed 5.0
      :max-acc 3.0
      :max-force 8
      :prev-locs []
      })

(defn find-target 
  ;;what is the new target for the pacer's path?
  [p time]
  (let [r (:radius p)
        t time
        [x y :as location] [(* (Math/cos t) r) (* (Math/sin t) r)]]
   ;(str location)
   (assoc-in p [:target] location)
  ))

;;;TODO;;;
;;;make this work;;;
(defn seek-target 
  "calculates vector to target"
  [p]  
  (let [desired (mv/subtract (:target p) (:location p))
        desired (if (every? zero? desired)
                  [0 0]
                  (->> (mv/normalize desired)
                       (mv/multiply (:max-speed p))))
        steer (mv/subtract desired (:velocity p))
        steer-lim (mv/limit (:max-force p) steer)]
    (mv/add steer-lim (:acceleration p))))

(let [x (if 6 "yes" "no")])

(defn apply-force
  "pass in net force vector and pacer"
  [f p]
  (let [f (mv/divide f (:mass p))]
    (mv/add (:acceleration p) f)))

;;;should each force divide by mass, or is it OK to do it in the tot-force?

(defn tot-force 
  "takes vector of force vectors and the thing to apply to - returns net force"
  [forces p]
  (->> (map #(mv/limit (:max-acc p) %) forces)
       (reduce mv/add)))

;;(into [] (apply-force ptst [10 10]))
;;(tot-forces [[10 10] [20 5]] ptst)
(defn check-edges [p]
  (let [[x y] (:location p)
        ob-x (or (> x (q/width)) (< x 0))
        ob-y (or (> y (q/height)) (< y 0))]
    (or ob-x ob-y)))

(defn check-edges-tst [p]
  (let [[x y] (:location p)
        ob-x (or (> x 500) (< x 0))
        ob-y (or (> y 500) (< y 0))]
    (or ob-x ob-y)))

(defn update-loc [p forces]
  (let [prev (conj (:prev-locs p) (:location p))
        net-force (tot-force forces p)
        acc (apply-force net-force p)
        ;;acc (mv/limit (:max-speed p) acc)
        vel (mv/add (:velocity p) acc)
        vel (mv/limit (:max-speed p) vel)
        loc (mv/add (:location p) vel)]
    (-> p
        (assoc :prev-locs prev)
        (assoc :acceleration acc)
        (assoc :velocity vel)
        (assoc :location loc)
        (assoc :acceleration [0.0 0.0]))))

;;test that the prev locs gets filled
;;(take 3 (iterate p/update-loc ptst))

(defn update-pacer [p]
  (let [[x y] (:location p)
        x (+ x (->> (q/random-gaussian)
                   (* 10)))
        y (+ y (->> (q/random-gaussian)
                   (* 10)))
        prev (conj (:prev-locs p) (:location p))]
   (assoc p :location [x y] 
            :prev-locs prev)))

(defn display-pacer [p]
  (let [[x y] (:location p)
        [a b] (or (last (:prev-locs p)) [x y])]
    ;;(println (str "Prev Loc: " x ":" y "  " a ":" b))
    ;;(q/ellipse x y 10 10)
    (q/line a b x y)
    ))

;;;;;;;;;;;;;;;
(defn tsts [p] (comp (fn [p] (conj (:prev-locs p) (:location p)))
      (fn [p] (assoc-in p [:location] [p]))))

(def ptst (new-pacer [40 20]))

(defn update-pacer-old [p]
  (let [[x y] (:location p)
        x (+ x (q/random -5 5))
        y (+ y (q/random -25 25))]
    (#(-> %
          (into (:prev-locs p) (:location p))
          (assoc-in [:location] [x y])))))