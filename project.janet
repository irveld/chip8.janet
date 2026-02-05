(declare-project
  :name "emu"
  :author "irveld"
  :license "BSD-3"
  :dependencies
  ["https://github.com/janet-lang/spork.git"
   "https://github.com/janet-lang/jaylib.git"])

(declare-source
  :prefix "emu"
  :source ["src/chip.janet"])

(declare-binscript :main "src/emu"
  :hardcode-syspath true)
