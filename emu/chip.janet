(import emu/display)
(import emu/input)
(use emu/config)
(use emu/opcodes)

### Initialization

(def- font
  (buffer/from-bytes
    0xF0 0x90 0x90 0x90 0xF0   # 0
    0x20 0x60 0x20 0x20 0x70   # 1
    0xF0 0x10 0xF0 0x80 0xF0   # 2
    0xF0 0x10 0xF0 0x10 0xF0   # 3
    0x90 0x90 0xF0 0x10 0x10   # 4
    0xF0 0x80 0xF0 0x10 0xF0   # 5
    0xF0 0x80 0xF0 0x90 0xF0   # 6
    0xF0 0x10 0x20 0x40 0x40   # 7
    0xF0 0x90 0xF0 0x90 0xF0   # 8
    0xF0 0x90 0xF0 0x10 0xF0   # 9
    0xF0 0x90 0xF0 0x90 0x90   # A
    0xE0 0x90 0xE0 0x90 0xE0   # B
    0xF0 0x80 0x80 0x80 0xF0   # C
    0xE0 0x90 0x90 0x90 0xE0   # D
    0xF0 0x80 0xF0 0x80 0xF0   # E
    0xF0 0x80 0xF0 0x80 0x80)) # F

(defn- load [rom &opt width height]
  (default width 64)
  (default height 32)
  @{:mem (-> (buffer/new-filled 4096 0)
           (buffer/blit font 0x050)
           (buffer/blit rom 0x200))
    :stack (array/new 16)

    :display (buffer/new-filled (* width height) 0)
    :width width
    :height height

    :V (buffer/new-filled 16 0)
    :I 0x000

    :PC 0x200
    :SP 0x0

    :delay 100
    :sound 20

    :keypad (array/new-filled 16 false)})

### Main cycle

(defn- fetch [chip]
  (defn nibble []
    ((chip :mem) (dec (++ (chip :PC)))))
  (let [ab (blshift (nibble) 8)
        cd (nibble)]
   (bor ab cd)))

(defn- decode [chip op]
  (let [[nnn kk n y x] [(band op 0x0FFF)
                        (band op 0x00FF)
                        (band op 0x000F)
                        (brshift (band op 0x00F0) 4)
                        (brshift (band op 0x0F00) 8)]
        nibbles (seq [shift :down-to (12 0 4)]
                  (band 0x000F (brshift op shift)))]
    (match nibbles
       [0 0 0xE 0] [op-00E0]
       [0 0 0xE 0xE] [op-00EE]
       [1 _ _ _] [op-1nnn nnn]
       [2 _ _ _] [op-2nnn nnn]
       [3 _ _ _] [op-3xkk x kk]
       [4 _ _ _] [op-4xkk x kk]
       [5 _ _ 0] [op-5xy0 x y]
       [6 _ _ _] [op-6xkk x kk]
       [7 _ _ _] [op-7xkk x kk]
       [8 _ _ 0] [op-8xy0 x y]
       [8 _ _ 1] [op-8xy1 x y]
       [8 _ _ 2] [op-8xy2 x y]
       [8 _ _ 3] [op-8xy3 x y]
       [8 _ _ 4] [op-8xy4 x y]
       [8 _ _ 5] [op-8xy5 x y]
       [8 _ _ 6] [op-8xy6 x y]
       [8 _ _ 7] [op-8xy7 x y]
       [8 _ _ 0xE] [op-8xyE x y]
       [9 _ _ 0] [op-9xy0 x y]
       [0xA _ _ _] [op-Annn nnn]
       [0xB _ _ _] [op-Bnnn nnn]
       [0xD _ _ _] [op-Dxyn x y n]
       [0xC _ _ _] [op-Cxkk x kk]
       [0xE _ 9 0xE] [op-Ex9E x]
       [0xE _ 0xA 1] [op-ExA1 x]
       [0xF _ 0 7] [op-Fx07 x]
       [0xF _ 0 0xA] [op-Fx0A x]
       [0xF _ 1 5] [op-Fx15 x]
       [0xF _ 1 8] [op-Fx18 x]
       [0xF _ 1 0xE] [op-Fx1E x]
       [0xF _ 2 9] [op-Fx29 x]
       [0xF _ 3 3] [op-Fx33 x]
       [0xF _ 5 5] [op-Fx55 x]
       [0xF _ 6 5] [op-Fx65 x])))

(defn- execute [chip [f & args]]
  (f chip ;args))

(defn- tick [chip]
  (defn decrement [timer]
    (unless (zero? (chip timer))
      (-- (chip timer))))
  (map decrement [:delay :sound])
  chip)

(defn run [rom-path]
  (let [rom (slurp rom-path)
        chip (load rom)
        [w h] (map chip [:width :height])]
      (var time 0)
      (display/with-window (* +scale+ w) (* +scale+ h) rom-path
        (ev/sleep (/ 1 +framerate+))
        (->> chip
          (display/render)
          (input/handle)
          (tick))
        (repeat +clock+
          (->>
            (fetch chip)
            (decode chip)
            (execute chip))))))
