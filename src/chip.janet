(use prelude/macros)

### Initialization

(def [+width+ +height+] [64 32])

(defn load [rom]
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

(defn access [chip dst &opt val]
  (if (nil? val)
    (get-in chip dst)
    (put-in chip dst val)))

(defn addr [chip nnn &opt val]
  (access chip [:mem nnn] val))

(defn V [chip &opt reg val]
  (if (nil? reg)
    (access chip [:V] val)
    (access chip [:V reg] val)))

(defn PC [chip &opt val]
  (access chip [:PC] val))

(defn I [chip &opt val]
  (access chip [:I] val))

(defn pixel [chip &opt pos action]
  (def buf (chip :display))
  (if (nil? pos) buf
    (let [i (display/coord->index ;pos +width+ +height+)]
      (case action
        :toggle (buffer/bit-toggle buf i)
        :on (buffer/bit-set buf i)
        :off (buffer/bit-clear buf i)
        (buffer/bit buf i)))))

(defmacro with-chip [chip & body]
  ~(let [fs [addr V PC I pixel]
         [addr V PC I pixel] (map |(partial $ ,chip) fs)]
     ,;body))

### Opcodes

(defn op-00E0 [chip]
  (print "CLS")
  (buffer/fill (chip :display) 0))

(defn op-1nnn [chip nnn]
  (printf "JP 0x%03X" nnn)
  (with-chip chip
    (PC nnn)))

(defn op-6xkk [chip x kk]
  (printf "LD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x kk)))

(defn op-7xkk [chip x kk]
  (printf "ADD V%X, 0x%02X" x kk)
  (with-chip chip
    (V x (+ (V x) kk))))

### Main cycle

(defn fetch [chip]
  (defn nibble []
    (let [pc (dec (++ (chip :PC)))]
      ((chip :mem) pc)))
  (defn byte []
    (bor (blshift (nibble) 8) (nibble)))
  (generate [op :iterate (byte) :until (zero? op)]
    op))

(defn execute [chip op]
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
      [1 _ _ _] [op-1nnn nnn]
      [6 _ _ _] [op-6xkk x kk]
      [7 _ _ _] [op-7xkk x kk]
      _ [identity]))
 (instr chip ;args))

(defn tick [chip]
  :TODO)

(defn render [chip]
  :TODO)

(defn cycle [chip op]
  (ev/sleep (/ 1 60))
  (doto chip
    (execute op)
    (tick)
    (render)))

(defn run [rom]
  (def chip (load rom))
  (map (partial cycle chip) (fetch chip)))
