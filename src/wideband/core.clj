(ns wideband.core
  (:gen-class)
  (:require [helins.linux.gpio :as gpio]))

(defn convert-to-binary
  [value]
  (map (comp {\1 1 \0 0} char) (Long/toString value 2)))

(defn convert-from-binary
  [value]
  (Long/parseLong (str value) 2))

(defn add-or-remove-bits
  "Adds or removes extra bits according to a bit limit"
  [bits limit]
  (let [ov-under (- limit (count bits))] ;; ov-under checks for how many bits are required to meet the limit
    (cond
      (> ov-under 0) (concat (repeat ov-under 0) bits)
      (< ov-under 0) (drop (* ov-under -1) bits)
      :else bits))) ;; Add exception for bits over limits

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

(defn bit-read
  "Function that will pass and value to the FPGA and read the result"
  [register]
  (with-open [device (gpio/device "/dev/gpiochip0")
              fpga-read (gpio/handle device
                                     {8 {:gpio/state true
                                         :gpio/tag :chip-select}
                                      11 {:gpio/state false
                                          :gpio/tag :clock}
                                      10 {:gpio/state false
                                          :gpio/tag :data-in}
                                      9 {:gpio/state false
                                         :gpio/tag :data-out}}
                                     {:gpio/direction :output})]
    (let [buffer (gpio/buffer fpga-read)
          high true
          low false]
      (gpio/write fpga-read
                  (gpio/set-line+ buffer {:chip-select low}))
      (Thread/sleep 1)
      (doseq [bit (generate-cmd-str :read register 0)]
        (gpio/write fpga-read
                    (gpio/set-line+ buffer {:data-in bit}))
        (gpio/read fpga-read
                   (gpio/get-line buffer :data-out))
        (Thread/sleep 1)
        (gpio/write fpga-read
                    (gpio/set-line+ buffer {:clock high}))
        (gpio/read fpga-read
                   (gpio/get-line buffer :data-out))
        (Thread/sleep 1)
        (gpio/write fpga-read
                    (gpio/set-line+ buffer {:clock low}))
        (gpio/read fpga-read
                   (gpio/get-line buffer :data-out))
        (Thread/sleep 1))
      (gpio/write fpga-read
                  (gpio/set-line+ buffer {:chip-select high})))))

(defn bit-bang
  "Function that will pass values over to the FPGA via bit banging"
  [register data]
  (with-open [device (gpio/device "/dev/gpiochip0")
              fpga-handle (gpio/handle device
                                       {8 {:gpio/state true
                                           :gpio/tag :chip-select}
                                        11 {:gpio/state false
                                            :gpio/tag :clock}
                                        10 {:gpio/state false
                                            :gpio/tag :data-in}}
                                       {:gpio/direction :output})]
    (let [buffer (gpio/buffer fpga-handle)
          high true
          low false]
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select low}))
      (Thread/sleep 1)
      (doseq [bit (generate-cmd-str "write" register data)]
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:data-in bit}))
        (Thread/sleep 1)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock high}))
        (Thread/sleep 1)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock low}))
        (Thread/sleep 1))
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select high})))))


(defn -main [& args]
  (println "Starting up GPIO test"))




