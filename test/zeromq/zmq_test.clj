(ns zeromq.zmq-test
  (:require [zeromq.zmq :as zmq]
            [zeromq.sendable :as s])
  (:use clojure.test))

(defonce context (zmq/zcontext))

(deftest push-pull-test
  (with-open [push (doto (zmq/socket context :push)
                     (zmq/connect "tcp://127.0.0.1:12349"))
              pull (doto (zmq/socket context :pull)
                     (zmq/bind "tcp://*:12349"))]
    (s/send "hello" push 0)
    (let [actual (String. (zmq/receive pull))]
      (is (= "hello" actual)))))


(deftest receive-str-timeout-test
  (with-open [pull (doto (zmq/socket context :pull)
                     (zmq/set-receive-timeout 100)
                     (zmq/bind "tcp://*:12310"))]
    (let [actual (zmq/receive-str pull)]
      (is (= nil actual)))))


(deftest dealer-router-test
  (with-open [dealer (doto (zmq/socket context :dealer)
                       (zmq/set-receive-timeout 3000)
                       (zmq/bind "tcp://*:12350"))
              router (doto (zmq/socket context :router)
                       (zmq/connect "tcp://127.0.0.1:12350"))]
    (s/send "myid" dealer zmq/send-more)
    (s/send "payload" dealer 0)
    (let [[id & _] (zmq/receive-all router)]
      (zmq/send router id zmq/send-more)
      (zmq/send-str router "ack")
      (let [actual (zmq/receive-str dealer)]
        (is (= "ack" actual))))))

(deftest plain-text-req-rep-test
  (let [server-key (zmq/curve-key-pair)
        client-key (zmq/curve-key-pair)]
    (with-open [req (doto (zmq/socket context :req)
                      (zmq/set-receive-timeout 3000)
                      (zmq/connect "tcp://127.0.0.1:12352"))
                rep (doto (zmq/socket context :rep)
                      (zmq/bind "tcp://127.0.0.1:12352"))]
      (let [success (s/send "payload" req 0)]
        (is success))
      (let [[_] (zmq/receive-all rep)]
        (zmq/send-str rep "ack")
        (let [actual (zmq/receive-str req)]
          (is (= "ack" actual)))))))

(deftest encrypted-req-rep-test
  (let [server-key (zmq/curve-key-pair)
        client-key (zmq/curve-key-pair)]
    (with-open [req (doto (zmq/socket context :req)
                      (zmq/set-receive-timeout 3000)
                      (zmq/make-into-curve-client
                       client-key
                       (.publicKey server-key))
                      (zmq/connect "tcp://127.0.0.1:12352"))
                rep (doto (zmq/socket context :rep)
                      (zmq/make-into-curve-server (.privateKey server-key))
                      (zmq/bind "tcp://127.0.0.1:12352"))]
      (println "Sending initial encrypted handshake request")
      (let [success (zmq/send-str req "payload" 0)]
        (is success))
      (println "Waiting for initial encrypted handshake")
      (let [[_] (zmq/receive-all rep)]
        (println "Answering encrypted handshake")
        (zmq/send-str rep "ack")
        (println "Waiting for response to encrypted handshake")
        (let [actual (zmq/receive-str req)]
          (is (= "ack" actual)))))))
