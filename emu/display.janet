(import jaylib :as jl)

(def +scale+ 10)

### --- Helpers ------------------------------------------

(defn coord->index [x y width height]
  (let [[px py] (map % [x y] [width height])
        index (+ px (* py width))]
    index))

(defn- rgb255->rgb1 [r g b]
  (map |(/ $ 255) [r g b]))

(defn- palette [key]
  (let [col {:fg [221 39 66]
             :bg [20 20 20]}]
    (rgb255->rgb1 ;(col key))))

### --- Display ------------------------------------------

(defn render [chip]
  (def [buf w h] (map chip [:display :width :height]))

  (defn draw? [x y]
    (buffer/bit buf (coord->index x y w h)))

  (defn square [x y]
    (let [dimensions [+scale+ +scale+]
          pos (map * dimensions [x y])
          color (palette :fg)]
      (jl/draw-rectangle-v pos dimensions color)))

  (loop [y :range [0 h]
         x :range [0 w]
         :when (draw? x y)]
    (square x y))

  chip)

(defmacro with-window [width height title & body]
  ~(do
    (,jl/init-window ,width ,height ,title)
    (,jl/hide-cursor)

    (while (not (,jl/window-should-close))
      (,jl/begin-drawing)
      (,jl/clear-background (,palette :bg))

      ,;body

      (,jl/end-drawing))
    (,jl/close-window)))
