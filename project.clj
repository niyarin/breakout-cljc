(defproject breakout-cljc "0.1.0-SNAPSHOT"
  :repositories [["clojars" {:url "https://clojars.org/repo"
                             :sign-releases false}]]
  :clean-targets ^{:protect false} ["target"]
  :main breakout-cljc.start
  :aot [breakout-cljc.start])
