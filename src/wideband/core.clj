(ns wideband.core
  (:gen-class)
  (:require [helins.linux.gpio :as gpio]))

(defn run []
  (with-open [device (gpio/device "/dev/gpiochip0")
            led-handle (gpio/handle device
                                    {5 {:gpio/state false
                                        :gpio/tag :led1}
                                     12 {:gpio/state false
                                         :gpio/tag :led2}}
                                    {:gpio/direction :output})
            button-watcher (gpio/watcher device
                                         {6 {:gpio/tag :led1-button
                                             :gpio/direction :input}})]
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
          (println "event" (gpio/event button-watcher))
          (println "AFTER EVENT<<==")
          (Thread/sleep 200)
          (println "recycling")
          (recur (dec counter) (rest leds))))))))

(defn -main [& args]
  (println "Starting up GPIO test")
  (run))




