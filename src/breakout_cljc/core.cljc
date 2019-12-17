(ns breakout-cljc.core
  (:require [breakout-cljc.utils :as utils]
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
                       :gameover false
                       :point 0
                       :pressed-keys #{}}))

(defmacro load-font-cljs [game bitmap]
   (let [{:keys [width height] :as bitmap'} bitmap]
      `(let [image# (js/Image. ~width ~height)]
         (doto image#
            (-> .-src (set! ~(text/bitmap->data-uri bitmap')))
            (-> .-onload (set! #({:bitmap {:data image# :width ~width :height ~height} :font ~(text/->baked-font "ttf/Roboto-Regular" 32 bitmap)})))))))

(defn load-font-clj [game bitmap]
   {:bitmap bitmap
     :font (text/->baked-font "ttf/Roboto-Regular.ttf" 32 bitmap)})

(defn load-font [game]
   (let [bitmap (text/->bitmap 512 512)] 
      #?(:clj (load-font-clj game bitmap)
         :cljs (load-font-cljs  game bitmap))))


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


  (let [{:keys [font bitmap]} (load-font game)
        font-entity 
        (->> (gl-text/->font-entity game (:data bitmap) font)
             (c/compile game))
        point-entity
           (->> (gl-text/->text-entity game font-entity "0")
                (c/compile game))
        gameover-entity
           (->> (gl-text/->text-entity game font-entity "Game Over")
                     (c/compile game)) ]
        (swap! *state assoc :point-entity point-entity)
        (swap! *state assoc :gameover-entity gameover-entity)
        (swap! *state assoc :font-entity font-entity))

  (let [paddle-entity
        (-> (c/compile game (e/->entity game p2d/rect))
            (assoc :width 80)
            (assoc :height 10)
            (t/color [1 1 1 1]))]
     (swap! *state assoc :paddle-entity paddle-entity)))

(def screen-entity
  {:viewport {:x 0 :y 0 :width 0 :height 0}
   :clear {:color [ 0.3 0.3 0.3 1] :depth 1}})


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
                point-entity
                font-entity
                text-entity
                point
                gameover
                gameover-entity
                block-width
                block-height
                ball-entity
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
      
      (when (not gameover)
         (doseq [block-entity block-entities]
             (when (:alive block-entity)
                (c/render game 
                       (-> (t/translate  
                              (t/project  block-entity game-width game-height)
                              (:px block-entity) (:py block-entity))
                           (t/scale 40 10)))))
         (c/render game
                 (-> (t/project paddle-entity game-width game-height)
                      (t/translate 
                         paddle-x
                         paddle-y)
                      (t/scale (:width paddle-entity) 10)))
         (c/render game
               (-> (t/project ball-entity game-width game-height)
                   (t/translate (:x ball-entity) (:y ball-entity))
                   (t/scale (:r ball-entity) (:r ball-entity))))
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
         
          (let [point' (* (count (filter #(not (:alive %)) block-entities)) 
                          100)]
               (when (not= point point')
                  (swap! *state
                         assoc
                         :point-entity 
                         (->> (gl-text/->text-entity game 
                                                     font-entity (str point'))
                              (c/compile game))
                         :point point'))
               (c/render game
                  (-> (t/project point-entity game-width game-height)
                      (t/scale (:width point-entity) (:height point-entity))
                      (t/translate 0 0)))))
      (when gameover
         (c/render game
           (-> (t/project gameover-entity game-width game-height)
               (t/scale (:width gameover-entity)
                        (:height gameover-entity))
               (t/translate 0 0))))))
  ;; return the game map
  game)
