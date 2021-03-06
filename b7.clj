(ns b7 (:use [overtone.live][mud.core][mud.chords]) (:require [shadertone.tone :as t] [mud.timing :as time]))



(ctl-global-clock 0.5)

(ctl time/root-s :rate 4.)


(defsynth data-probes [timing-signal-bus 0]
  (let [beat-count (in:kr timing-signal-bus)
        _ (tap "global-beat-count" 60 (a2k beat-count))]
    (out 0 0)))

(def active-data-probes (data-probes (:count time/beat-1th)))


(def active-color (atom 0.0))

(defsynth in-bus-synth [in-bus 0 gain 10 cutoff 10]
  (let [src (sound-in in-bus)
        srci (lpf src cutoff)
        srco (* gain srci)
        _ (tap "ivol" 60 srco)]
    (out 0 (pan2 srco))))

(def ibs (in-bus-synth))

(ctl ibs :cutoff 40)

(kill ibs)


(def in-bus-atom (atom {:synth ibs :tap "ivol"}))

(def in-bus-tap (get-in (:synth @in-bus-atom) [:taps (:tap @in-bus-atom)]))

(add-watch in-bus-tap :level
           (fn [_ _ old new]
             (when (< 0.05 new)
               (t/set-video-frame 2 51000)
               ;(bassSin)
               (overpad 15 :attack 0.1 :release 0.025)
               (overpad 10 :attack 0.01 :release 0.1)
               )))

(stop)


(defsynth humm [amp 1 f0 19 f1 20 f2 30 f3 40 f4 50]
  (let [src (apply + (* (sin-osc [f0 f1 f2 f3 f4]) 0.2))
        src2 (sin-osc 0.1)
        outt (* amp src src2)
        _ (tap "outt" 60 outt)]
    (out 0 (pan2 outt))))

(def hummf (humm))

(ctl hummf :amp 1 :f1 19 :f2 20 :f3 40)

(kill hummf)


;(def ws (t/getWindowState))

;(def rh (nth (:redHistogram @ws) 2))

;(def bh (nth (:blueHistogram @ws) 2))

(def ch (t/get-cam-histogram 1 :red) )


(add-watch ch :ch (fn [_ _ old new]
                                       ; (println (nth @rh 200))
                    (ctl hummf :f4 (nth @ch 100))
                    (ctl hummf :f3 (nth @ch 200))
                    ;(t/set-video-frame 2 (nth @ch 20))
                    ) )
(t/set-video-play 2)

;(t/start "./b7.glsl" :width 1920 :height 1080 :cams [0]  :videos ["./jkl.mp4" "./metro.mp4" "./spede.mp4"] :user-data {"iGlobalBeatCount" (atom {:synth active-data-probes :tap "global-beat-count"}) "iActiveColor" active-color "iA" (atom {:synth dummyf :tap "a"}) "iB" (atom {:synth dummyf :tap "b"}) "iC" (atom {:synth dummyf :tap "c"}) "iD" (atom {:synth dummyf :tap "d"}) "iE" (atom {:synth dummyf :tap "e"}) "outt" (atom {:synth hummf :tap "outt"})  })


(t/start "./b7.glsl" :width 1920 :height 1080 :cams [0 1] :videos ["./jkl.mp4" "./metro.mp4" "./spede.mp4"])


(defsynth dualPulse [note 44 amp 1 attack 0.1 decay 0.1 sustain 0.2 release 1.4]
  (let [;freq (* 0.02 (sin-osc 100) (saw note))
        sp1 (sin-osc 10 0)
        sp2 (pulse 1 0.1)
        env (env-gen (perc attack release) :action FREE)
        src2 (sin-osc 20 (* Math/PI 0.5 sp2))]
    (out 0 (pan2 (* env amp (* sp1 sp1 src2))))))


(def dualPulsef (dualPulse))

(kill dualPulsef)

(def my-bus0 (control-bus 1))
(control-bus-set! my-bus0 10101)

(defsynth noise [freq 44 amp 1 freq2 44]
  (let [noiseV (pink-noise)
        src1 (sin-osc noiseV)
        src2 (sin-osc (* noiseV 0.9 (in:kr freq2)))
        src3 (lf-saw freq)]
    (out 0 (pan2 (*  amp (+ src1 src2) src3)))))

(def noisef (noise :freq2 my-bus0))

(kill noisef)

(defsynth overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq (midicps note)
        env (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 10 freq (env-gen (perc 0.012 (- release 0.01)))))
        bfreq (/ freq 2)
        sig (apply - (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)])) (lpf (saw [freq (* freq 1.01)]) f-env)))
        _ (tap "sig" 60 (a2k sig))]
    (out 0 (pan2 (* amp env sig)))))

(overpad 30 :attack 0.01 :release 0.1)

(overpad 20 :attack 0.01 :release 0.1)


(defsynth bassSin [amp 0.25 freq1 20 phase1 0 freq2 40 phase2 0 freq3 9]
  (let [freq-env (env-gen (perc 0.1 0.1) :action FREE)
        src1 (sin-osc (* freq1 freq-env) phase1)
        src2 (sin-osc freq1 phase2)
        src3 (lf-saw (* freq3 freq-env))]
    (out 0 (pan2 (clip2 (+ (* amp 0.0125 src3) (* 0.5 amp src1 src2 freq-env)) 1)))))

(def bassSinf (bassSin))

(kill bassSinf)

(defsynth kick [amp 5.7 freq 80 fw 10 phase1 1]
  (let [src (sin-osc freq phase1)
        sawsrc (saw fw)
        env (env-gen (perc 1.2 1.6) :action FREE)
        _ (tap "kicksaw" 60 (a2k sawsrc))
        ] (out 0 (pan2 (* env sawsrc)))))



(def kickf (kick :amp 1 :fw 6))


(defonce kick-drum-buffer (buffer 256))

(pattern! kick-drum-buffer [1 0 0 0 0 1 0 1 1 0 1 0])

(defsynth drum-data-probe [kick-drum-buffer 0 timing-signal-bus 0]
  (let [beat-count (in:kr timing-signal-bus)
        drum-beat (buf-rd:kr 1 kick-drum-buffer beat-count)
        _ (tap "drum-beat" 60 (a2k drum-beat))]
    (out 0 0)))



(def drum-data-probef (drum-data-probe kick-drum-buffer (:count time/beat-1th)))

(kill drum-data-probef)

(def kick-atom (atom {:synth drum-data-probef :tap "drum-beat"}))

(def kick-tap (get-in (:synth @kick-atom) [:taps (:tap @kick-atom)]))

(def set-frames [52000 51000 200])




;(add-watch (:redHistogram shadertone.to))

(add-watch kick-tap :cell-color
           (fn [_ _ old new]
             (when (and (= old 0.0) (= 1.0 new))
               ;(t/set-video-frame 2 (nth set-frames (mod (+ @active-color 1.0) (count set-frames))))
               (bassSin)
               (overpad 15 :attack 0.01 :release 0.25)
               (overpad 5 :attack 0.1 :release 0.1)
               (reset! active-color (mod (+ @active-color 1.0) 100))
               )))


(t/set-video-fps 1 25)

(t/set-video-frame 2 52000)

(t/post-start-cam 0)




(def sauna-buf (load-sample "./sauna-session-short.wav"))

(defsynth sauna []
  (let [dry (play-buf 1 sauna-buf)
	wet (free-verb dry 1)]
    (out 0 (pan2 [wet dry]))))

(sauna)

(stop)


(defsynth overpad_c [note 60 amp 0.7 attack 0.01 release 2]
  (let [freq (midicps note)
        f-env  (+ freq (* 10 freq (env-gen (perc 0.012 (- release 0.01)))))
        bfreq (/ freq 2)
        sig (apply - (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)])) (lpf (saw [freq (* freq 1.01)]) f-env)))]
    (out 0 (pan2 (* amp sig)))))

;(def opc (overpad_c 30 :attack 10 :release 20 :amp 1))


(do
  (Thread/sleep 20000)
  (t/set-video-frame 0 9000)
  (def opc (overpad_c 30 :attack 10 :release 20 :amp 1))
  (ctl dummyf :b 1)
  (ctl dummyf :a 0)
    (Thread/sleep 5000)
  (ctl dummyf :b 0)
  (ctl dummyf :a 1)
  (sauna)
  (Thread/sleep 21000)
  (def kickf (kick :amp 1 :fw 6)))

(t/set-video-frame 0 9000)

(stop)
