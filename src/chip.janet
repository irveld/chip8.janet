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
  (def [addr kk n y x]
    [|(band op 0x0FFF)
     |(band op 0x00FF)
     |(band op 0x000F)
     |(brshift (band op 0x00F0) 4)
     |(brshift (band op 0x0F00) 8)])
  :TODO
  (printf "%04X :: (PC=%04X)"
    op
    (- (chip :PC) 2)))

(defn tick [chip]
  :TODO)

(defn render [chip]
  :TODO)

(defn cycle [chip op]
  (doto chip
    (execute op)
    (tick)
    (render)))

(defn run [rom]
  (def chip (load rom))
  (map (partial cycle chip) (fetch chip)))
