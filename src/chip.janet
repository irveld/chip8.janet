(def [+width+ +height+] [64 32])

(defn load [rom]
  @{:memory (buffer/blit (buffer/new-filled 4096 0) rom 0x200)
    :stack (array/new 16)

    :display (buffer/new-filled (* +width+ +height+) 0)

    :V (buffer/new-filled 16 0)
    :I 0x000

    :PC 0x200
    :SP 0x0

    :delay 100
    :sound 20})

(defn run [rom]  # TODO:
  (def chip (load rom))
  :TODO)
