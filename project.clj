(defproject overtime "0.1.0-SNAPSHOT"
  :description "Library for Overtone/SuperCollider to control sound events over time"
  :url "http://github.com/shj333/overtime"
  :license {:name         "The MIT License (MIT)"
            :url          "http://opensource.org/licenses/MIT"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [overtone "0.10.1"]
                 [seesaw "1.4.5"]
                 [org.clojure/tools.logging "0.3.1"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]])
