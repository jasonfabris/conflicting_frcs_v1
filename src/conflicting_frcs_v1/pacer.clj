(ns conflicting_frcs_v1.pacer
  (:require [conflicting_frcs_v1.math.vector :as mv]
            [quil.core :as q]))

(defn new-pacer [loc]
  ;make a new pacer
     {:location loc
      :velocity [1 1]
      :acceleration [10 10]
      :mass 1.0
      :radius 100.0
      :target loc
      :max-speed 10.0
      :max-force 10.0
      :prev-locs []
      }
)

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
(defn seek-target [p]
  (let [
        desired (mv/subtract (:target p) (:location p))
        desired (mv/normalize desired)
        desired (mv/multiply desired (:max-speed p))
        steer (mv/subtract desired (:velocity p))
        steer (mv/limit steer (:max-force p))]
    (-> p
        (update-in [:acceleration] #(mv/add % steer)))))

(defn apply-force-old [p f]
  (let [f (mv/divide f (:mass p))]
    (update-in p [:acceleration] #(mv/add % f))))


(defn apply-force
  "pass in net force vector and pacer"
  [f p]
  (let [f (mv/divide f (:mass p))]
    (mv/add (:acceleration p) f)))

(defn tot-forces 
  "takes vector of force vectors and the thing to apply to - returns net force"
  [forces p]
  (let [net-frc (reduce mv/add forces)]  
    (apply-force net-frc p)))

;;(into [] (apply-force ptst [10 10]))
;;(tot-forces [[10 10] [20 5]] ptst)

(defn update-loc [p forces]
  (let [prev (conj (:prev-locs p) (:location p))
        net-force (reduce mv/add forces)
        acc (apply-force net-force p)
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
  (let [[x y] (:location p)]
    (q/ellipse x y 10 10)))

;;;;;;;;;;;;;;;
(defn tsts [p] (comp (fn [p] (conj (:prev-locs p) (:location p)))
      (fn [p] (assoc-in p [:location] [p]))))

(def ptst (new-pacer [10 20]))

(defn update-pacer-old [p]
  (let [[x y] (:location p)
        x (+ x (q/random -5 5))
        y (+ y (q/random -25 25))]
    (#(-> %
          (into (:prev-locs p) (:location p))
          (assoc-in [:location] [x y])))))