(use prelude/macros)

(import jaylib :as :jl)

(import emu/display)

(def [+width+ +height+] [64 32])
(def +scale+ 10)

(def +rng+ (math/rng 0)) # NOTE: Maybe use proper seed later

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

(defn- load [rom]
  @{:mem (-> (buffer/new-filled 4096 0)
           (buffer/blit font 0x050)
           (buffer/blit rom 0x200))
    :stack (array/new 16)

    :display (buffer/new-filled (* +width+ +height+) 0)

    :V (buffer/new-filled 16 0)
    :I 0x000

    :PC 0x200
    :SP 0x0

    :delay 100
    :sound 20

    :keypad (array/new-filled 16 false)})

### Helpers for reading and writing chip data

(defn- access [chip dst &opt val]
  (if (nil? val)
    (get-in chip dst)
    (put-in chip dst val)))

(defn- mem [chip &opt val]
  (access chip [:mem] val))

(defn- addr [chip nnn &opt val]
  (access chip [:mem nnn] val))

(defn- stack [chip &opt val]
  (access chip [:stack (chip :SP)] val))

(defn- V [chip &opt reg val]
  (if (nil? reg)
    (access chip [:V] val)
    (access chip [:V reg] val)))

(defn- PC [chip &opt val]
  (access chip [:PC] val))

(defn- SP [chip &opt val]
  (access chip [:SP] val))

(defn- I [chip &opt val]
  (access chip [:I] val))

(defn- display [chip &opt val]
  (access chip [:display] val))

(defn- pixel [chip &opt pos action]
  (def buf (chip :display))
  (if (nil? pos) buf
    (let [i (display/coord->index ;pos +width+ +height+)]
      (case action
        :toggle (buffer/bit-toggle buf i)
        :on (buffer/bit-set buf i)
        :off (buffer/bit-clear buf i)
        (buffer/bit buf i)))))

(defn- timer [chip name &opt val]
  (access chip [name] val))

(defn- keypad [chip &opt key]
  (if (nil? key)
   (access chip [:keypad])
   (access chip [:keypad key])))

(defn- skip [chip]
  (+= (chip :PC) 2))

(defmacro- with-chip [chip & body]
  ~(let [fs [mem addr stack
              V PC SP I
              pixel timer keypad skip display]
          f |(partial $ ,chip)
          [mem addr stack
           V PC SP I
           pixel timer keypad skip display] (map f fs)]
      ,;body))

(defmacro defop [op args & body]
  (def $chip (gensym))
  ~(defn ,op [,$chip ,;args]
     (with-chip ,$chip
       ,;body)))

### Opcodes

(defop op-00E0 []
 (buffer/fill (display) 0))

(defop op-00EE []
  (PC (stack))
  (SP (dec (SP))))

(defop op-1nnn [nnn]
  (PC nnn))

(defop op-2nnn [nnn]
  (SP (inc (SP)))
  (stack (PC))
  (PC nnn))

(defop op-3xkk [x kk]
 (when (= (V x) kk) (skip)))

(defop op-4xkk [x kk]
  (when (not= (V x) kk) (skip)))

(defop op-5xy0 [x y]
 (when (= (V x) (V y)) (skip)))

(defop op-6xkk [x kk]
  (V x kk))

(defop op-7xkk [x kk]
  (V x (+ (V x) kk)))

(defop op-8xy0 [x y]
  (V x (V y)))

(defop op-8xy1 [x y]
  (V x (bor (V x) (V y))))

(defop op-8xy2 [x y]
  (V x (band (V x) (V y)))
  (V 0xF 0))

(defop op-8xy3 [x y]
  (V x (bxor (V x) (V y)))
  (V 0xF 0))

(defop op-8xy4 [x y]
  (let [result (+ (V x) (V y))
        carry (if (> result 255) 1 0)]
    (V x result)
    (V 0xF carry)))

(defop op-8xy5 [x y]
  (let [[Vx Vy] (map V [x y])
        result (- Vx Vy)
        borrow (if (>= Vx Vy) 1 0)]
    (V x result)
    (V 0xF borrow)))

(defop op-8xy6 [x y]
  (V x (V y))
  (let [Vx (V x)
        sig-bit (band 1 Vx)]
    (V x (brshift Vx 1))
    (V 0xF sig-bit)))

(defop op-8xy7 [x y]
  (let [[Vx Vy] (map V [x y])
        result (- Vy Vx)
        borrow (if (>= Vy Vx) 1 0)]
    (V x result)
    (V 0xF borrow)))

(defop op-8xyE [x y]
  (V x (V y))
  (let [Vx (V x)
        sig-bit (band 1 (brshift Vx 7))]
    (V x (blshift Vx 1))
    (V 0xF sig-bit)))

(defop op-9xy0 [x y]
  (when (not= (V x) (V y))
    (skip)))

(defop op-Annn [nnn]
  (I nnn))

(defop op-Bnnn [nnn]
  (PC (+ nnn (V 0))))

(defop op-Cxkk [x byte]
  (let [rand (math/rng-int +rng+ 256)]
    (V x (band byte rand))))

(defop op-Dxyn [x y n]
  (defn sprite-bit? [sprite col]
    (not (zero? (band sprite (brshift 0x80 col)))))
  (V 0xF 0)
  (loop [row :range [0 n]
         :let [sprite (addr (+ row (I)))]
         col :range [0 8]
         :when (sprite-bit? sprite col)
         :let [[Vx Vy] (map V [x y])
               pos (map + [col row] [Vx Vy])]]
    (when (pixel pos)
      (V 0xF 1))
    (pixel pos :toggle)))

(defop op-Ex9E [x]
  (when (keypad (V x))
    (PC (+ (PC) 2))))

(defop op-ExA1 [x]
  (unless (keypad (V x))
    (PC (+ (PC) 2))))

(defop op-Fx0A [x]
  (if-let [held |(when (keypad $) $)
           key (some held (keys (keypad)))]
    (V x key)
    (PC (- (PC) 2))))

(defop op-Fx07 [x]
  (V x (timer :delay)))

(defop op-Fx15 [x]
  (timer :delay (V x)))

(defop op-Fx18 [x]
  (timer :sound (V x)))

(defop op-Fx1E [x]
  (I (+ (I) (V x))))

(defop op-Fx29 [x]
  (let [start 0x050
        offset (* 5 (V x))]
    (I (+ start offset))))

(defop op-Fx33 [x]
  (let [Vx (V x) I (I)]
   (addr (+ I 0) (div (mod Vx 1000) 100))
   (addr (+ I 1) (div (mod Vx 100) 10))
   (addr (+ I 2) (div (mod Vx 10) 1))))

(defop op-Fx55 [x]
  (let [start 0
        end (inc x)]
    (buffer/blit (mem) (V) (I) start end)
    (I (+ 1 (I) x))))

(defop op-Fx65 [x]
  (let [start (I)
        end (+ start (inc x))]
    (buffer/blit (V) (mem) 0 start end)
    (I (+ 1 (I) x))))

### Main cycle

(defn- fetch [chip]
  (defn nibble []
    ((chip :mem) (dec (++ (chip :PC)))))
  (let [ab (blshift (nibble) 8)
        cd (nibble)]
   (bor ab cd)))

(def- actual-key
  (zipcoll
    [0x1 0x2 0x3 0xC  # Chip8 keys (emulated)
     0x4 0x5 0x6 0xD
     0x7 0x8 0x9 0xE
     0xA 0x0 0xB 0xF]
    [:1  :2  :3  :4   # Input keys (from user)
     :q  :w  :e  :r
     :a  :s  :d  :f
     :z  :x  :c  :v]))

(defn- input [chip]
  (loop [:let (keypad (chip :keypad))
         k :in (keys keypad)
         :let [pressed? (jl/key-down? (actual-key k))]]
    (put keypad k pressed?)))

(defn- execute [chip op]
  (let [[nnn kk n y x] [(band op 0x0FFF)
                        (band op 0x00FF)
                        (band op 0x000F)
                        (brshift (band op 0x00F0) 4)
                        (brshift (band op 0x0F00) 8)]
        nibbles (seq [shift :down-to (12 0 4)]
                  (band 0x000F (brshift op shift)))]
    (def [instr & args]
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
         [0xF _ 6 5] [op-Fx65 x]
         _ [identity]))
    (instr chip ;args)))

(defn- tick [chip]
  (defn decrement [timer]
    (unless (zero? (chip timer))
      (-- (chip timer))))
  (map decrement [:delay :sound]))

(defn- render [chip]
  (display/draw (chip :display)
    +width+ +height+
    +scale+))

(defn- cycle [chip]
  (render chip)
  (input chip)
  (repeat 10
    (execute chip (fetch chip))))

(defn run [rom-path]
  (let [rom (slurp rom-path)
        chip (load rom)
        [w h] (map * [+scale+ +scale+] [+width+ +height+])]
    (var time 0)
    (display/with-window w h rom-path
      (cycle chip)
      (let [period (/ 1 60)
            delta (jl/get-frame-time)]
        (when (< period (+= time delta))
          (tick chip)
          (set time 0))))))
