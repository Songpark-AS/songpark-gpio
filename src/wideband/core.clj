(ns wideband.core
  (:gen-class)
  (:require [helins.linux.gpio :as gpio]))

(defn convert-to-binary
  [value]
  (map (comp {\1 1 \0 0} char) (Long/toString value 2)))

(defn add-or-remove-bits
  "Adds or removes extra bits according to a bit limit"
  [bits limit]
  (let [ov-under (- limit (count bits))] ;; ov-under checks for how many bits are required to meet the limit
    (cond
      (> ov-under 0) (do
                       (concat (repeat ov-under 0) bits))
      (< ov-under 0) (do
                       (drop (* ov-under -1) bits))
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
  (if (= "read" op)
    (concat [0 0] (get-data-address-bits register 6) (get-data-address-bits value 8))
    (concat [0 1] (get-data-address-bits register 6) (get-data-address-bits value 8))))

(defn bit-bang
  "Function that will pass values over to the FPGA via bit banging"
  [register data]
  (with-open [device (gpio/device "/dev/gpiochip0")
              fpga-handle (gpio/handle device
                                       {8 {:gpio/state true
                                           :gpio/tag :chip-select}
                                        11 {:gpio/state false
                                            :gpio/tag :clock}
                                        9 {:gpio/state false
                                           :gpio/tag :data-in}}
                                       {:gpio/direction :input})]
    (let [buffer (gpio/buffer fpga-handle)]
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select false}))
      (Thread/sleep 1)
      (doseq [bit (generate-cmd-str "write" register data)]
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:data-in bit}))
        (Thread/sleep 1)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock true}))
        (Thread/sleep 1)
        (gpio/write fpga-handle
                    (gpio/set-line+ buffer {:clock false}))
        (Thread/sleep 1))
      (gpio/write fpga-handle
                  (gpio/set-line+ buffer {:chip-select true})))))


(defn -main [& args]
  (println "Starting up GPIO test"))




