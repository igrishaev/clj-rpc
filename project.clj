(defproject jsonrpc "0.1.0-SNAPSHOT"

  :description "FIXME: write description"

  :url "http://example.com/FIXME"

  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}

  :dependencies [[clj-http "3.12.0"]
                 [cheshire "5.10.0"]
                 [org.clojure/tools.logging "1.1.0"]

                 [ring/ring-jetty-adapter "1.7.1"]
                 [ring/ring-json "0.5.0"]

                 [ch.qos.logback/logback-classic "1.2.3"]]

  :main ^:skip-aot jsonrpc.core

  :target-path "target/%s"

  :profiles
  {:dev
   {:dependencies [[org.clojure/clojure "1.10.1"]]}

   :uberjar
   {:aot :all
    :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
