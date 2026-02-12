(import emu/display)
(use emu/config)

### --- Helpers ------------------------------------------

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
  (let [[buf w h] (map chip [:display :width :height])
        i (display/coord->index ;pos w h)]
    (if (nil? pos) buf
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

# TODO: Fix this mess
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

### --- Opcodes ------------------------------------------

# Clear the display
(defop op-00E0 []
  (buffer/fill (display) 0))

# Return from current subroutine
(defop op-00EE []
  (PC (stack))
  (SP (dec (SP))))

# Jump to address nnn
(defop op-1nnn [nnn]
  (PC nnn))

# Call subroutine at address nnn
(defop op-2nnn [nnn]
  (SP (inc (SP)))
  (stack (PC))
  (PC nnn))

# Skip next instruction if Vx = kk
(defop op-3xkk [x kk]
  (when (= (V x) kk) (skip)))

# Skip next instruction if Vx != kk
(defop op-4xkk [x kk]
  (when (not= (V x) kk) (skip)))

# Skip next instruction if Vx = Vy
(defop op-5xy0 [x y]
  (when (= (V x) (V y)) (skip)))

# Vx = kk
(defop op-6xkk [x kk]
  (V x kk))

# Vx += kk
(defop op-7xkk [x kk]
  (V x (+ (V x) kk)))

# Vx = Vy
(defop op-8xy0 [x y]
  (V x (V y)))

# Vx = Vx OR Vy
(defop op-8xy1 [x y]
  (V x (bor (V x) (V y))))

# Vx = Vx AND Vy, then reset the flag register
(defop op-8xy2 [x y]
  (V x (band (V x) (V y)))
  (V 0xF 0))

# Vx = Vx XOR Vy, then reset the flag register
(defop op-8xy3 [x y]
  (V x (bxor (V x) (V y)))
  (V 0xF 0))

# Vx += Vy, then indicate carry in flag register
(defop op-8xy4 [x y]
  (let [result (+ (V x) (V y))
        carry (if (> result 255) 1 0)]
    (V x result)
    (V 0xF carry)))

# Vx -= Vy, then indicate -no borrow- in flag register
(defop op-8xy5 [x y]
  (let [[Vx Vy] (map V [x y])
        result (- Vx Vy)
        borrow (if (>= Vx Vy) 1 0)]
    (V x result)
    (V 0xF borrow)))

# Vx = Vy, shift Vx left by 1, then store LSB in flag register
(defop op-8xy6 [x y]
  (V x (V y))
  (let [Vx (V x)
        lsb (band 1 Vx)]
    (V x (brshift Vx 1))
    (V 0xF lsb)))

# Vx = Vy - Vx, then indicate -no borrow- in flag register
(defop op-8xy7 [x y]
  (let [[Vx Vy] (map V [x y])
        result (- Vy Vx)
        borrow (if (>= Vy Vx) 1 0)]
    (V x result)
    (V 0xF borrow)))

# Vx = Vy, shift Vx left by 1, then store MSB in flag register
(defop op-8xyE [x y]
  (V x (V y))
  (let [Vx (V x)
        msb (band 1 (brshift Vx 7))]
    (V x (blshift Vx 1))
    (V 0xF msb)))

# Skip next instruction if Vx != Vy
(defop op-9xy0 [x y]
  (when (not= (V x) (V y)) (skip)))

# I = nnn
(defop op-Annn [nnn]
  (I nnn))

# Jump to address (nnn + V0)
(defop op-Bnnn [nnn]
  (PC (+ nnn (V 0))))

# Vx = kk AND (random byte)
(defop op-Cxkk [x byte]
  (let [rand (math/rng-int +rng+ 256)]
    (V x (band byte rand))))

# Draw n bytes from address I at coordinates (Vx, Vy),
# then indicate collision in flag register
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

# Skip next instruction if key Vx is pressed
(defop op-Ex9E [x]
  (when (keypad (V x))
    (PC (+ (PC) 2))))

# Skip next instruction if key Vx is not pressed
(defop op-ExA1 [x]
  (unless (keypad (V x))
    (PC (+ (PC) 2))))

# Vx = Delay timer
(defop op-Fx07 [x]
  (V x (timer :delay)))

# Vx = (key of next keypress)
(defop op-Fx0A [x]
  (if-let [held |(when (keypad $) $)
           key (some held (keys (keypad)))]
    (V x key)
    (PC (- (PC) 2))))

# Delay timer = Vx
(defop op-Fx15 [x]
  (timer :delay (V x)))

# Sound timer = Vx
(defop op-Fx18 [x]
  (timer :sound (V x)))

# I += Vx
(defop op-Fx1E [x]
  (I (+ (I) (V x))))

# I = (address of font sprite for digit corresponding to Vx)
(defop op-Fx29 [x]
  (let [start 0x050
        offset (* 5 (V x))]
    (I (+ start offset))))

# Set I, I+1, I+2 to the 100's, 10's, and 1's digits of Vx
(defop op-Fx33 [x]
  (let [Vx (V x) I (I)]
    (addr (+ I 0) (div (mod Vx 1000) 100))
    (addr (+ I 1) (div (mod Vx 100) 10))
    (addr (+ I 2) (div (mod Vx 10) 1))))

# Set values at I ... (I + x + 1) to V0 ... Vx
(defop op-Fx55 [x]
  (let [start 0
        end (inc x)]
    (buffer/blit (mem) (V) (I) start end)
    (I (+ 1 (I) x))))

# Set V0...Vx to values at I...(I + x + 1)
(defop op-Fx65 [x]
  (let [start (I)
        end (+ start (inc x))]
    (buffer/blit (V) (mem) 0 start end)
    (I (+ 1 (I) x))))

