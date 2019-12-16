(ns breakout-cljc.core
  (:require [breakout-cljc.utils :as utils]
            ;[breakout-cljc.move :as move]
            [play-cljc.gl.core :as c]
            [play-cljc.gl.entities-2d :as e]
            [play-cljc.text :as text]
            [play-cljc.gl.text :as gl-text]
            [play-cljc.transforms :as t]
            [play-cljc.primitives-2d :as p2d]
            #?(:clj  [play-cljc.macros-java :refer [gl math]]
               :cljs [play-cljc.macros-js :refer-macros [gl math]])))

(defonce *state (atom {:mouse-x 0
                       :mouse-y 0
                       :paddle-y 320
                       :block-width 40
                       :block-height 10
                       :pressed-keys #{}
                       :x-velocity 0
                       :y-velocity 0
                       :can-jump? false
                       :direction :right
                       :player-images {}
                       :player-image-key :walk1}))

(defn init [game]
  ;; allow transparency in images
  (gl game enable (gl game BLEND))
  (gl game blendFunc (gl game SRC_ALPHA) (gl game ONE_MINUS_SRC_ALPHA))
  (let [block-entities
         (for [x (range 10) y (range 5)]
             (-> (c/compile game (e/->entity game p2d/rect))
                 (t/color [(rand) (rand) (rand) 1])
                 (assoc :px (+ 40 (* x 40)))
                 (assoc :py (+ 20 (* y 10)))
                 (assoc :alive true)))]
     (swap! *state 
            assoc 
            :block-entities
            block-entities))

  (let [ball (-> (c/compile game (e/->entity game p2d/rect))
                 (assoc :vx 2.0 :vy 1.0)
                 (assoc :x 200.0 :y 150.0)
                 (assoc :r 10)
                 (t/color [1 1 1 1]))]
     (swap! *state assoc :ball-entity ball))


  (let [font (text/->baked-font "ttf/m.ttf" 20 (text/->bitmap 512 512))
        font-entity (gl-text/->font-entity game "TEST" font)
        ;text-entity (c/compile game (gl-text/->text-entity game font "HELLO" ))
        ]
        
        ;(swap! *state assoc :ball-entity ball))
     )

  (let [paddle-entity
        (-> (c/compile game (e/->entity game p2d/rect))
            (assoc :width 80)
            (assoc :height 10)
            (t/color [1 1 1 1]))]
     (swap! *state assoc :paddle-entity paddle-entity)))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [ 0 0 0 1] :depth 1}})


(defn update-ball [state]
   (let [ball (:ball-entity state)
         {:keys [x y vx vy]} ball]
      (assoc state
         :ball-entity
         (assoc ball
            :x (+ x vx)
            :y (+ y vy)))))

(defn collision-wall [state game-width game-height]
   (let [ball (:ball-entity state)
         {:keys [x y vx vy r]} ball
         new-vx (cond 
                    (<= x 0) (- vx)
                    (>= (+ x r) game-width) (- vx)
                    :default vx)
         new-vy (cond
                   (<= y 0) (- vy)
                   :default vy)]
      (assoc state
             :ball-entity
             (assoc ball
                    :vx new-vx
                    :vy new-vy))))

(defn collision-paddle [state paddle-x paddle-y paddle-width paddle-height]
   (let [ball (:ball-entity state)
         {:keys [x y vx vy r]} ball
         [new-vx new-vy]
               (cond 
                   (and (<= paddle-x (+ x r))
                        (<= x (+ paddle-x (/ paddle-width 2)))
                        (<= paddle-y (+ y r))
                        (<= y (+ paddle-y paddle-height))) 
                     [(if (neg? vx) vx (- vx)) (- vy)]
                    (and (<= (+ paddle-x (/ paddle-width 2)) (+ x r))
                        (<= x (+ paddle-x paddle-width))
                        (<= paddle-y (+ y r))
                        (<= y (+ paddle-y paddle-height)))
                     [(if (neg? vx) (- vx) vx) (- vy)]
                     :default [vx vy])]
      (assoc state
             :ball-entity
             (assoc ball
                    :vx new-vx
                    :vy new-vy))))

(defn collision-blocks [state]
   (let [ball (:ball-entity state)
         {:keys [x y vx vy r]} ball
         block-entities (:block-entities state)
         block-width (:block-width state)
         block-height (:block-height state)
         [hit-block new-vx new-vy new-x new-y]
         (first
            (->> block-entities
                 (map 
                     #(let [{:keys [px py]} %
                           block-left px
                           block-right (+ px block-width)
                           block-top py
                           block-bottom (+ py block-height) ]
                        (and
                           (:alive %)
                           (<= block-left (+ x r))
                             (<= x block-right)
                             (<= block-top (+ y r))
                             (<= y block-bottom)
                             [%
                              (if (and (< block-top y)
                                       (< y block-bottom))
                                 (- vx) vx)
                              (if (and (< block-left x)
                                       (< x block-right))
                                 (- vy) vy)
                              (+ x (- vx))
                              (+ y (- vy))
                              ])))
                     (drop-while not)))
         new-block-entities
         (map #(if (= % hit-block) (assoc % :alive false) %)
              block-entities)]
      (if hit-block
       (assoc state
             :ball-entity
             (assoc ball
                    :vx (if new-vx new-vx vx)
                    :vy (if new-vy new-vy vy)
                    :x new-x
                    :y new-y)
             :block-entities
                new-block-entities)
       state))
       )

(defn tick [game]
  (let [{:keys [pressed-keys
                player-x
                player-y
                mouse-x
                paddle-entity
                paddle-y
                block-width
                block-height
                ball-entity
                ;text-entity
                direction
                block-entities]
         :as state} @*state
        game-width (utils/get-width game)
        game-height (utils/get-height game)
        paddle-x
        (->> (- mouse-x (/ (:width paddle-entity) 2))
            (max 0)
            (min (- game-width (:width paddle-entity)))) ]
    (when (and (pos? game-width) (pos? game-height))
      ;; render the blue background
      (c/render game (update screen-entity :viewport
                             assoc :width game-width :height game-height))
      
       (doseq [block-entity block-entities]
          (when (:alive block-entity)
             (c/render game 
                    (->
                (t/translate  (t/project  block-entity game-width game-height)
                             (:px block-entity) (:py block-entity))
                   (t/scale 40 10)))))

          (c/render game
                 (-> (t/project paddle-entity game-width game-height)
                      (t/translate 
                         paddle-x
                         paddle-y)
                      (t/scale (:width paddle-entity) 10)
                      ) )

          (c/render game
               (-> (t/project ball-entity game-width game-height)
                   (t/translate (:x ball-entity) (:y ball-entity))
                   (t/scale (:r ball-entity) (:r ball-entity))))

          ;(c/render game
               ;text-entity)
          (swap! *state
              (fn [state]
                 (-> state
                     update-ball 
                     (collision-wall game-width game-height)
                     (collision-blocks)
                     (collision-paddle paddle-x
                                       paddle-y 
                                       (:width paddle-entity) 
                                       (:height paddle-entity)))))
         
          (let [point 
                (* (count (filter #(not (:alive %)) block-entities))
                   100)]
             (clojure.pprint/pprint point))


          ;; change the state to move the player
          '(swap! *state
            (fn [state]
              (->> (assoc state
                          :player-width player-width
                          :player-height player-height)
                   (move/prevent-move game)
                   (move/animate game)
                   (move/move game)
                   )))
          ))
  ;; return the game map
  game)
