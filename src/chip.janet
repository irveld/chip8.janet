(use prelude/macros)

(import jaylib :as :jl)

(import emu/display)

(def [+width+ +height+] [64 32])
(def +scale+ 10)

(def +log+ false)
(defn log [& xs]
  (when +log+ (print ;xs)))
(defn logf [fmt & xs]
  (when +log+ (printf fmt ;xs)))

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

(defn- keypad [chip key]
  (access chip [:keypad key]))

(defn skip [chip]
  (+= (chip :PC) 2))

(defmacro- with-chip [chip & body]
  ~(let [fs [mem addr stack V PC SP I pixel timer keypad skip]
         f |(partial $ ,chip)
         [mem addr stack V PC SP I
          pixel timer keypad skip] (map f fs)]
     ,;body))

### Opcodes

(defn- op-00E0 [chip]
  (log "CLS")
  (buffer/fill (chip :display) 0))

(defn- op-00EE [chip]
  (log "RET")
  (with-chip chip
    (PC (stack))
    (SP (dec (SP)))))

(defn- op-1nnn [chip nnn]
  (logf "JP 0x%03X" nnn)
  (with-chip chip
    (PC nnn)))

(defn- op-2nnn [chip nnn]
  (logf "CALL 0x%03X" nnn)
  (with-chip chip
    (SP (inc (SP)))
    (stack (PC))
    (PC nnn)))

(defn- op-3xkk [chip x kk]
  (logf "SE V%02X, 0x%03X" x kk)
  (with-chip chip
    (when (= (V x) kk)
      (skip))))

(defn- op-4xkk [chip x kk]
  (logf "SNE V%02X, 0x%03X" x kk)
  (with-chip chip
    (when (not= (V x) kk)
      (skip))))

(defn- op-5xy0 [chip x y]
  (logf "SE V%02X, V%02X" x y)
  (with-chip chip
    (when (= (V x) (V y))
      (skip))))

(defn- op-6xkk [chip x kk]
  (logf "LD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x kk)))

(defn- op-7xkk [chip x kk]
  (logf "ADD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x (+ (V x) kk))))

(defn- op-8xy0 [chip x y]
  (logf "LD V%X, V%X" x y)
  (with-chip chip
    (V x (V y))))

(defn- op-8xy1 [chip x y]
  (logf "OR V%X, V%X" x y)
  (with-chip chip
    (V x (bor (V x) (V y)))))

(defn- op-8xy2 [chip x y]
  (logf "AND V%X, V%X" x y)
  (with-chip chip
    (V x (band (V x) (V y)))))

(defn- op-8xy3 [chip x y]
  (logf "XOR V%X, V%X" x y)
  (with-chip chip
    (V x (bxor (V x) (V y)))))

(defn- op-8xy4 [chip x y]
  (logf "ADD V%X, V%X" x y)
  (with-chip chip
    (let [result (+ (V x) (V y))
          carry (if (> result 255) 1 0)]
      (V x result)
      (V 0xF carry))))

(defn- op-8xy5 [chip x y]
  (logf "SUB V%X, V%X" x y)
  (with-chip chip
    (let [[Vx Vy] (map V [x y])
          result (- Vx Vy)
          borrow (if (>= Vx Vy) 1 0)]
      (V x result)
      (V 0xF borrow))))

(defn- op-8xy6 [chip x y] # TODO: document + option for 'quirk'
  (logf "SHR V%X{, V%X}" x y)
  (with-chip chip
    (let [Vx (V x)
          sig-bit (band 1 Vx)]
      (V x (brshift Vx 1))
      (V 0xF sig-bit))))

(defn- op-8xy7 [chip x y]
  (logf "SUBN V%X, V%X" x y)
  (with-chip chip
    (let [[Vx Vy] (map V [x y])
          result (- Vy Vx)
          borrow (if (>= Vy Vx) 1 0)]
      (V x result)
      (V 0xF borrow))))

(defn- op-8xyE [chip x y] # TODO: document + option for 'quirk'
  (logf "SHL V%X{, V%X}" x y)
  (with-chip chip
    (let [Vx (V x)
          sig-bit (band 1 (brshift Vx 7))]
      (V x (blshift Vx 1))
      (V 0xF sig-bit))))

(defn- op-9xy0 [chip x y]
  (logf "SNE V%02X, V%02X" x y)
  (with-chip chip
    (when (not= (V x) (V y))
      (skip))))

(defn- op-Annn [chip nnn]
  (logf "LD I, 0x%03X" nnn)
  (with-chip chip
    (I nnn)))

(defn- op-Bnnn [chip nnn]
  (logf "JP V0, 0x%03X" nnn)
  (with-chip chip
    (PC (+ nnn (V 0)))))

(defn- op-Cxkk [chip x kk]
  (logf "RND V%X, 0x%02X" x kk)
  (with-chip chip
    (let [rand (math/rng-int +rng+ 256)]
      (V x (band kk rand)))))

(defn- op-Dxyn [chip x y n]
  (logf "DRW V%X, V%X, 0x%X" x y n)
  (defn sprite-bit? [sprite col]
    (not (zero? (band sprite (brshift 0x80 col)))))
  (with-chip chip
    (V 0xF 0)
    (loop [row :range [0 n]
           :let [sprite (addr (+ row (I)))]
           col :range [0 8]
           :when (sprite-bit? sprite col)
           :let [[Vx Vy] (map V [x y])
                 pos (map + [col row] [Vx Vy])]]
      (when (pixel pos)
        (V 0xF 1))
      (pixel pos :toggle))))

(defn- op-Ex9E [chip x]
  (logf "SKP V%X" x)
  (with-chip chip
    (when (keypad (V x))
      (PC (+ (PC) 2)))))

(defn- op-ExA1 [chip x]
  (logf "SKNP V%X" x)
  (with-chip chip
    (unless (keypad (V x))
      (PC (+ (PC) 2)))))

(defn- op-Fx0A [chip x]
  (logf "LD V%X, K" x)
  (with-chip chip
    (if-let [held |(when (keypad $) $)
             key (some held (keys (chip :keypad)))]
      (V x key)
      (PC (- (PC) 2)))))

(defn- op-Fx07 [chip x]
  (logf "LD V%X, DT" x)
  (with-chip chip
    (V x (timer :delay))))

(defn- op-Fx15 [chip x] x
  (logf "LD DT, V%X" x)
  (with-chip chip
    (timer :delay (V x))))

(defn- op-Fx18 [chip x] x
  (logf "LD ST, V%X" x)
  (with-chip chip
    (timer :sound (V x))))

(defn- op-Fx1E [chip x]
  (logf "ADD I, V%X" x)
  (with-chip chip
    (I (+ (I) (V x)))))

(defn- op-Fx29 [chip x]
  (logf "LD F, V%X" x)
  (with-chip chip
    (let [start 0x050
          offset (* 5 (V x))]
      (I (+ start offset)))))

(defn- op-Fx33 [chip x]
  (logf "LD B, V%X" x)
  (with-chip chip
    (let [I (I) Vx (V x)]
     (addr I (div (mod Vx 1000) 100))
     (addr (+ I 1) (div (mod Vx 100) 10))
     (addr (+ I 2) (div (mod Vx 10) 1)))))

(defn- op-Fx55 [chip x]
  (logf "LD [I], V%X" x)
  (with-chip chip
    (let [start 0
          end (inc x)]
      (buffer/blit (mem) (V) (I) start end))))

(defn- op-Fx65 [chip x]
  (logf "LD V%X, [I]" x)
  (with-chip chip
    (let [start (I)
          end (+ start (inc x))]
      (buffer/blit (V) (mem) 0 start end))))

### Main cycle

(defn- fetch [chip]
  (defn nibble []
    (let [pc (dec (++ (chip :PC)))]
      ((chip :mem) pc)))
  (defn byte []
    (bor (blshift (nibble) 8) (nibble)))
  (generate [op :iterate (byte) :until (zero? op)]
    op))

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
  (def [nnn kk n y x]
    [(band op 0x0FFF)
     (band op 0x00FF)
     (band op 0x000F)
     (brshift (band op 0x00F0) 4)
     (brshift (band op 0x0F00) 8)])
 (def nibbles
   (seq [shift :down-to (12 0 4)]
     (band 0x000F (brshift op shift))))
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
 (instr chip ;args))

(defn- tick [chip]
  (defn decrement [timer]
    (unless (zero? (chip timer))
      (-- (chip timer))))
  (map decrement [:delay :sound]))

(defn- render [chip]
  (display/draw (chip :display)
    +width+ +height+
    +scale+))

(defn run [rom-path]
  (let [rom (slurp rom-path)
        chip (load rom)
        ops (fetch chip)
        [w h] (map * [+scale+ +scale+] [+width+ +height+])]
    (var time 0)
    (display/with-window w h rom-path
      (input chip)

      (when (< (/ 1 60) time)
        (tick chip)
        (set time 0))

      (repeat 10
        (when-let [op (resume ops)]
          (execute chip op)))

      (render chip)

      (+= time (jl/get-frame-time)))))
