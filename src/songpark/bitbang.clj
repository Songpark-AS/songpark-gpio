(ns songpark.bitbang
  (:require [helins.linux.gpio :as gpio])
  (:import [java.lang AutoCloseable]))

(defn convert-to-binary
  [value]
  (map (comp {\1 1 \0 0} char) (Long/toString value 2)))

(defn convert-from-binary
  [value]
  (Long/parseLong (clojure.string/join value) 2))

(defn add-or-remove-bits
  "Adds or removes extra bits according to a bit limit"
  [bits limit]
  (let [num (count bits)
        ov-under (- limit (count bits))]
    (cond
      (= limit 8) (cond ;; Data shou
                    (> num limit) (throw (AssertionError. "Data value is out of range"))
                    (< num limit) (concat (repeat ov-under 0) bits)
                    :else bits)
      (= limit 6) (cond
                    (> num limit) (drop (* ov-under -1) bits)
                    (< num limit) (concat (repeat ov-under 0) bits)
                    :else bits)))) ;; Add exception for bits over limits

(defn get-data-address-bits
  "creates the bits portion, returns 6 bits for address and 8 for data"
  [value mode]
  (let [bits (convert-to-binary value)
        num-bits (count (convert-to-binary value))]
    (if (> mode num-bits)
      (add-or-remove-bits bits mode)
      bits)))

(defn generate-cmd-str
  "generates the Instruction that will be provided sent to FPGA"
  [op register value]
  (let [rw-bits (if (= :read op)
                  [0 0]
                  [0 1])
        bits (concat rw-bits (get-data-address-bits register 6) (get-data-address-bits value 8))]
    ;; once we have the bits, we need to convert them to true and false,
    ;; as the GPIO library we use operates with booleans, and not 1 and 0
    (map {0 false 1 true} bits)))

(defn sleep [^Long delay]
  (let [start ^Long (System/nanoTime)]
    (while (> (+ start delay) (System/nanoTime)))))

(defn bit-reader
  "Function that will pass and value to the FPGA and read the result"
  [register cs clk d-in d-out]
  (with-open [device (gpio/device "/dev/gpiochip0")
              handle-write (gpio/handle device
                                        {cs {:gpio/state true ;; [chip-select, clock, data-in, data-out] make pins configurable 
                                             :gpio/tag :chip-select}
                                         clk {:gpio/state false
                                              :gpio/tag :clock}
                                         d-in {:gpio/state false
                                               :gpio/tag :data-in}}
                                        {:gpio/direction :output})
              handle-read (gpio/handle device
                                       {d-out {:gpio/state false
                                               :gpio/tag :data-out}}
                                       {:gpio/direction :input})]
    (let [buffer-write (gpio/buffer handle-write)
          buffer-read  (gpio/buffer handle-read)
          high true
          low false
          out (atom [])
          bits {false 0 true 1}]
      (gpio/write handle-write
                  (gpio/set-line+ buffer-write {:chip-select low}))
      (sleep 100)
      (doseq [bit (generate-cmd-str :read register 0)]
        (gpio/write handle-write
                    (gpio/set-line+ buffer-write {:data-in bit}))
        (gpio/read handle-read buffer-read)
        (swap! out conj (get bits (gpio/get-line buffer-read :data-out)))
        (sleep 100)
        (gpio/write handle-write
                    (gpio/set-line+ buffer-write {:clock high}))
        (sleep 100)
        (gpio/write handle-write
                    (gpio/set-line+ buffer-write {:clock low}))
        (sleep 100))
      (gpio/write handle-write
                  (gpio/set-line+ buffer-write {:chip-select high}))
      (drop 8 @out))))

(defn bit-read
  "Function that will return the FPGA value as decimal it uses bit reader"
  [register chip-select clock data-in data-out]
  (convert-from-binary (bit-reader register chip-select clock data-in data-out)))

(defn bit-write
  "Function that will pass values over to the FPGA via bit banging"
  [register data cs clk d-in]
  (with-open [device (gpio/device "/dev/gpiochip0")
              fpga-handle (gpio/handle device
                                       {cs {:gpio/state true
                                            :gpio/tag :chip-select}
                                        clk {:gpio/state false
                                             :gpio/tag :clock}
                                        d-in {:gpio/state false
                                              :gpio/tag :data-in}}
                                       {:gpio/direction :output})]
    (let [buffer (gpio/buffer fpga-handle)
          high true
          low false]
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select low}))
      (sleep 100)
      (doseq [bit (generate-cmd-str :write register data)]
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:data-in bit}))
        (sleep 100)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock high}))
        (sleep 100)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock low}))
        (sleep 100))
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select high})))))

