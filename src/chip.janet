(use prelude/macros)

(import emu/display)

(def [+width+ +height+] [64 32])
(def +scale+ 10)

### Initialization

(defn- load [rom]
  @{:mem (buffer/blit (buffer/new-filled 4096 0) rom 0x200)
    :stack (array/new 16)

    :display (buffer/new-filled (* +width+ +height+) 0)

    :V (buffer/new-filled 16 0)
    :I 0x000

    :PC 0x200
    :SP 0x0

    :delay 100
    :sound 20})

### Helpers for reading and writing chip data

(defn- access [chip dst &opt val]
  (if (nil? val)
    (get-in chip dst)
    (put-in chip dst val)))

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

(defmacro- with-chip [chip & body]
  ~(let [fs [addr stack V PC SP I pixel]
         [addr stack V PC SP I pixel] (map |(partial $ ,chip) fs)]
     ,;body))

### Opcodes

(defn- op-00E0 [chip]
  (printf "CLS")
  (buffer/fill (chip :display) 0))

(defn- op-00EE [chip]
  (printf "RET")
  (with-chip chip
    (PC (stack))
    (SP (dec (SP)))))

(defn- op-1nnn [chip nnn]
  (printf "JP 0x%03X" nnn)
  (with-chip chip
    (PC nnn)))

(defn- op-2nnn [chip nnn]
  (printf "CALL 0x%03X" nnn)
  (with-chip chip
   (SP (inc (SP)))
   (stack (PC))))

(defn- op-3xkk [chip x kk]
  (printf "SE V%02X, 0x%03X" x kk)
  (with-chip chip
    (when (= (V x) kk)
      (PC (+ (PC) 2)))))

(defn- op-4xkk [chip x kk]
  (printf "SNE V%02X, 0x%03X" x kk)
  (with-chip chip
    (when (not= (V x) kk)
      (PC (+ (PC) 2)))))

(defn- op-5xy0 [chip x y]
  (printf "SE V%02X, V%02X" x y)
  (with-chip chip
    (when (= (V x) (V y))
      (PC (+ (PC) 2)))))

(defn- op-6xkk [chip x kk]
  (printf "LD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x kk)))

(defn- op-7xkk [chip x kk]
  (printf "ADD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x (+ (V x) kk))))

(defn- op-9xy0 [chip x y]
  (printf "SNE V%02X, V%02X" x y)
  (with-chip chip
    (when (not= (V x) (V y))
      (PC (+ (PC) 2)))))

(defn- op-Annn [chip nnn]
  (printf "LD I, 0x%03X" nnn)
  (with-chip chip
    (I nnn)))

(defn- op-Bnnn [chip nnn]
  (printf "JP V0, 0x%03X" nnn)
  (with-chip chip
    (PC (+ nnn (V 0)))))

(defn- op-Dxyn [chip x y n]
  (printf "DRW V%X, V%X, 0x%X" x y n)
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

### Main cycle

(defn- fetch [chip]
  (defn nibble []
    (let [pc (dec (++ (chip :PC)))]
      ((chip :mem) pc)))
  (defn byte []
    (bor (blshift (nibble) 8) (nibble)))
  (generate [op :iterate (byte) :until (zero? op)]
    op))

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
 (print (string ;(map |(string/format "%X" $) nibbles)))
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
      [9 _ _ 0] [op-9xy0 x y]
      [0xA _ _ _] [op-Annn nnn]
      [0xB _ _ _] [op-Bnnn nnn]
      [0xD _ _ _] [op-Dxyn x y n]
      _ [identity]))
 (instr chip ;args))

(defn- tick [chip]
  :TODO)

(defn- render [chip]
  (display/draw (chip :display)
    +width+ +height+
    +scale+))

(defn- cycle [chip op]
  (ev/sleep (/ 1 60))
  (doto chip
    (execute op)
    (tick)
    (render)))

(defn run [rom-path]
  (let [rom (slurp rom-path)
        chip (load rom)
        ops (fetch chip)
        [w h] (map * [+scale+ +scale+] [+width+ +height+])]
    (display/with-window w h rom-path
      (when-let [op (resume ops)]
        (cycle chip op)))))
