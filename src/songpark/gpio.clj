(ns songpark.gpio
  (:require [clojure.set :as set]
            [helins.linux.gpio :as gpio])
  ;; AutoCloseable needs to be imported, otherwise the with-open macro hangs
  (:import [java.lang AutoCloseable]))


(defn set-led [gpio led on-off-value]
  (assert (#{:on :off} on-off-value) "on-off-value has to be either :on or :off")
  (assert (contains? @(:leds gpio) led) (str "led has to be one of " (keys @(:leds gpio))))
  (swap! (:leds gpio) assoc led on-off-value))

(defn get-led [gpio led]
  (get @(:leds/current-state gpio) led))

(defn set-leds [gpio leds]
  (assert (map? leds)
          "leds has to be a map with led keys and corrsesponding values of :on or :off")
  (assert (every? #{:on :off} (vals leds))
          "leds have to contain either :on or :off")
  (assert (set/subset? (set (keys leds))
                       (set (keys @(:leds gpio))))
          (str "leds can only contain the keys " (keys @(:leds gpio))))
  (reset! (:leds gpio) leds))

(defn set-button-action [gpio button function]
  (assert (fn? function) "function has to be a function")
  (assert (contains? @(:buttons gpio) button) (str "button has to be one of " (keys @(:buttons gpio))))
  (swap! assoc (:buttons gpio) button function))

(defn unset-button-action [gpio button]
  (assert (contains? @(:buttons gpio) button) (str "button has to be one of " (keys @(:buttons gpio))))
  (swap! assoc (:buttons gpio) button nil))

(defn close! [{:keys [running?] :as _gpio}]
  (reset! running? false))


(defrecord GPIO [f buttons leds device handles watchers])

(defn- get-writeable-leds [on-off-map gpio]
  (->> @(:leds gpio)
       (remove (fn [[_ v]]
                 (nil? v)))
       (map (fn [[k v]]
              [k (on-off-map v)]))
       (into {})))

(defn- reset-leds! [gpio]
  (let [ks (keys @(:leds gpio))]
    (reset! (:leds gpio) (->> (map vector ks (repeat nil))
                              (into {})))))

(defn- get-milliseconds
  ([nano-timestamp]
   (long (/ nano-timestamp 1000000)))
  ([nano-timestamp cast-to]
   (case cast-to
     :double (double (/ nano-timestamp 1000000))
     (long (/ nano-timestamp 1000000)))))

(defn- get-sleep-time [boundary end-time start-time]
  (max 1 (- boundary (get-milliseconds (- end-time start-time)))))

(defn- handle-buttons [{:keys [buttons watchers running?] :as gpio}]
  (future
    (try
      (let [bounce (atom {})
            ;; epsilon of 10 ms for bouncing signals
            epsilon 10.0]
        (while @running?
          ;; (println :handle-buttons)
          (let [event (gpio/event @watchers 10000)]
            (when event
              (let [{:gpio/keys [tag nano-timestamp edge]} event
                    rising (get-in @bounce [tag :rising])]
                ;; (println {:event event
                ;;           :bounce @bounce})
                (cond (and (nil? rising)
                           (= edge :rising))
                      (swap! bounce assoc-in [tag :rising] nano-timestamp)

                      (and (some? rising)
                           (= edge :falling))
                      (let [ms (get-milliseconds (- nano-timestamp rising) :double)]
                        (when (> ms epsilon)
                          (if-let [f (get @buttons tag)]
                            (do (f gpio ms)
                                (swap! bounce dissoc tag))
                            (do (println "Error. Could not find a function for " tag)
                                (swap! bounce dissoc tag)))))
                      
                      :else
                      :do-nothing))))))
      (catch Throwable t
        (println :handle-buttons/error t)))))

(defn get-gpio
  ([]
   (get-gpio nil))
  ([settings]
   (let [component (map->GPIO (merge {:buttons (atom {:button/push1 nil})
                                      :leds (atom {:led/red :off
                                                   :led/yellow :off
                                                   :led/green :off})
                                      :leds/current-state (atom {:led/red :off
                                                                 :led/yellow :off
                                                                 :led/green :off})
                                      :device (atom nil)
                                      :handles (atom nil)
                                      :watchers (atom nil)
                                      :running? (atom true)}
                                     settings))
         ;; for the LEDs false (ie, 0), is the LED turned on
         ;; and true (ie, 1), is the LED turned off
         ;; this is decided by the HW guys, and had to do with how much
         ;; power was drawn from the power card
         on-off-map {:on false :off true}]
     (future
       (try
         (with-open [device (gpio/device "/dev/gpiochip0")
                     handles (gpio/handle device
                                          {15 {:gpio/state true
                                               :gpio/tag :led/yellow}
                                           14 {:gpio/state true
                                               :gpio/tag :led/green}
                                           9 {:gpio/state true
                                              :gpio/tag :led/red}}
                                          {:gpio/direction :output})
                     watchers (gpio/watcher device
                                            {0 {:gpio/tag :button/push1
                                                :gpio/direction :input}})]
           (do (reset! (:device component) device)
               (reset! (:handles component) handles)
               (reset! (:watchers component) watchers))

           (handle-buttons component)
           (let [buffer (gpio/buffer handles)]
             (while @(:running? component)
               (let [start-time ^Long (System/nanoTime)
                     leds (get-writeable-leds on-off-map component)]
                 (when-not (empty? leds)
                   (gpio/write handles
                               (gpio/set-line+ buffer leds))
                   (swap! (:leds/current-state component) merge (->> (:leds component)
                                                                     deref
                                                                     (remove (fn [[_ v]]
                                                                               (nil? v)))))
                   (reset-leds! component))
                 (let [sleep-time (get-sleep-time 20 ^Long (System/nanoTime) start-time)]
                   ;;(println "Sleeping")
                   (Thread/sleep sleep-time))))))
         (catch Throwable t
           (println :get-gpio/error t))))
     component)))


(comment
  (def tmp (get-gpio {:buttons (atom {:button/push1
                                      (fn [gpio delay]
                                        (let [flip-value ({:on :off
                                                           :off :on}
                                                          (get-led gpio :led/red))]
                                          (println "Flipping :led/red to " flip-value)
                                          (set-led gpio :led/red flip-value))
                                        (println "Delay was " delay " milliseconds"))})}))

  (set-leds tmp {:led/yellow :off
                 :led/green :off
                 :led/red :off
                 })
  tmp
  (get-led tmp :led/red)
  (close! tmp)

  (get-milliseconds (- 1550151940942776762 1550151940942354770) :double)
  )
