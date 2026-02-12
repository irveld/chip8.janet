(import jaylib :as :jl)

(def- chip8->user
  (zipcoll
    [0x1 0x2 0x3 0xC  # Chip8 keys (emulated)
     0x4 0x5 0x6 0xD
     0x7 0x8 0x9 0xE
     0xA 0x0 0xB 0xF]
    [:1  :2  :3  :4   # Input keys (from user)
     :q  :w  :e  :r
     :a  :s  :d  :f
     :z  :x  :c  :v]))

(defn handle [chip]
  (let [keypad (chip :keypad)]
    (loop [key :in (keys (chip :keypad))]
      (put keypad key (jl/key-down? (chip8->user key)))))
  chip)

