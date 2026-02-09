(import jaylib :as jl)

(defn coord->index [x y width height]
  (let [[px py] (map % [x y] [width height])
        index (+ x (* y width))]
    index))

(defn- rgb255->rgb1 [r g b]
  (map |(/ $ 255) [r g b]))

(defn- palette [key]
  (let [col {:fg [221 39 66]
             :bg [20 20 20]}]
    (rgb255->rgb1 ;(col key))))

(defn draw [buf width height &opt scale]
  (default scale 1)

  (defn draw? [x y]
    (buffer/bit buf (coord->index x y width height)))

  (defn square [x y]
    (let [dimensions [scale scale]
          pos (map * dimensions [x y])
          color (palette :fg)]
      (jl/draw-rectangle-v pos dimensions color)))

  (loop [y :range [0 height]
         x :range [0 width]
         :when (draw? x y)]
    (square x y)))

(defmacro with-window [width height title & body]
  ~(do
    (,jl/init-window ,width ,height ,title)
    (,jl/set-target-fps 60)
    (,jl/hide-cursor)

    (while (not (,jl/window-should-close))
      (,jl/begin-drawing)
      (,jl/clear-background (,palette :bg))

      ,;body

      (,jl/end-drawing))
    (,jl/close-window)))
