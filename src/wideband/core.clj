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
      :else bits)))

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
                                       {8 {:gpio/state false
                                           :gpio/tag :chip-select}
                                        11 {:gpio/state false
                                            :gpio/tag :clock}
                                        9 {:gpio/state false
                                           :gpio/tag :data-in}}
                                       {:gpio/direction :input})]
    (let [buffer (gpio/buffer fpga-handle)]
      (doseq [bit (generate-cmd-str "write" register data)]
        #_{:clj-kondo/ignore [:redundant-do]}
        (do
          (gpio/write fpga-handle
                      (gpio/set-line+ buffer {:chip-select false
                                              :clock false
                                              :data-in bit}))
          (gpio/write fpga-handle
                      (gpio/set-line+ buffer {:chip-select false
                                              :clock true
                                              :data-in bit}))
          (gpio/write fpga-handle
                      (gpio/set-line+ buffer {:chip-select false
                                              :clock false
                                              :data-in bit})))))))

(defn run-plus []
  (with-open [device         (gpio/device "/dev/gpiochip0")
              led-handle     (gpio/handle device
                                          {11 {:gpio/state false
                                               :gpio/tag   :led-1}
                                           8 {:gpio/state true
                                              :gpio/tag   :led-2}}
                                          {:gpio/direction :output})
              button-watcher (gpio/watcher device
                                           {10 {:gpio/direction :input}})]
    (let [buffer (gpio/buffer led-handle)]
      (loop [leds (cycle [:led-1
                          :led-2])]
        (gpio/write led-handle
                    (gpio/set-line+ buffer
                                    {(first  leds) true
                                     (second leds) false}))
        (gpio/event button-watcher)
        (recur (rest leds))))))

(defn run []
  (with-open [device (gpio/device "/dev/gpiochip0")
              led-handle (gpio/handle device
                                      {10 {:gpio/state false
                                           :gpio/tag :led1}
                                       8 {:gpio/state false
                                          :gpio/tag :led2}}
                                      {:gpio/direction :output})]
    (let [buffer (gpio/buffer led-handle)]
      (loop [counter 10
             leds (cycle [:led1 :led2])]
        (gpio/write led-handle
                    (gpio/set-line+ buffer {(first leds) true
                                            (second leds) false}))

        (if (zero? counter)
        ;; cleanup
          (gpio/write led-handle
                      (gpio/set-line+ buffer {(first leds) false
                                              (second leds) false}))
          (do
            (println "==>>BEFORE EVENT")
            ;; (println "event" (gpio/event button-watcher))
            (println "AFTER EVENT<<==")
            (Thread/sleep 200)
            (println "recycling")
            (recur (dec counter) (rest leds))))))))

(defn -main [& args]
  (println "Starting up GPIO test")
  (run-plus))




