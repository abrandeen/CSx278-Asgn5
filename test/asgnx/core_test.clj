(ns asgnx.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.async :refer [<!!]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.generators :as gen]
            [asgnx.core :refer :all]
            [asgnx.kvstore :as kvstore :refer [put! get!]]))



(deftest words-test
  (testing "that sentences can be split into their constituent words"
    (is (= ["a" "b" "c"] (words "a b c")))
    (is (= [] (words "   ")))
    (is (= [] (words nil)))
    (is (= ["a"] (words "a")))
    (is (= ["a"] (words "a ")))
    (is (= ["a" "b"] (words "a b")))))


(deftest cmd-test
  (testing "that commands can be parsed from text messages"
    (is (= "foo" (cmd "foo")))
    (is (= "foo" (cmd "foo x y")))
    (is (= nil   (cmd nil)))
    (is (= ""    (cmd "")))))


(deftest args-test
  (testing "that arguments can be parsed from text messages"
    (is (= ["x" "y"] (args "foo x y")))
    (is (= ["x"] (args "foo x")))
    (is (= [] (args "foo")))
    (is (= [] (args nil)))))


(deftest parsed-msg-test
  (testing "that text messages can be parsed into cmd/args data structures"
    (is (= {:cmd "foo"
            :args ["x" "y"]}
           (parsed-msg "foo x y")))
    (is (= {:cmd "foo"
            :args ["x"]}
           (parsed-msg "foo x")))
    (is (= {:cmd "foo"
            :args []}
           (parsed-msg "foo")))
    (is (= {:cmd "foo"
            :args ["x" "y" "z" "somereallylongthing"]}
           (parsed-msg "foo x y z somereallylongthing")))))

(deftest welcome-test
  (testing "that welcome messages are correctly formatted"
    (is (= "Welcome bob" (welcome {:cmd "welcome" :args ["bob"]})))
    (is (= "Welcome bob" (welcome {:cmd "welcome" :args ["bob" "smith"]})))
    (is (= "Welcome bob smith jr" (welcome {:cmd "welcome" :args ["bob smith jr"]})))))


(deftest create-router-test
  (testing "correct creation of a function to lookup a handler for a parsed message"
    (let [router (create-router {"hello" #(str (:cmd %) " " "test")
                                 "argc"  #(count (:args %))
                                 "echo"  identity
                                 "default" (fn [& a] "No!")})
          msg1   {:cmd "hello"}
          msg2   {:cmd "argc" :args [1 2 3]}
          msg3   {:cmd "echo" :args ["a" "z"]}
          msg4   {:cmd "echo2" :args ["a" "z"]}]
      (is (= "hello test" ((router msg1) msg1)))
      (is (= "No!" ((router msg4) msg4)))
      (is (= 3 ((router msg2) msg2)))
      (is (= msg3 ((router msg3) msg3))))))


(deftest action-send-msg-test
  (testing "That action send msg returns a correctly formatted map"
    (is (= :send
           (:action (action-send-msg :bob "foo"))))
    (is (= :bob
           (:to (action-send-msg :bob "foo"))))
    (is (= "foo"
           (:msg (action-send-msg [:a :b] "foo"))))))

(deftest action-insert-test
  (testing "That action insert returns a correctly formatted map"
    (is (= #{:action :ks :v}
           (into #{}(keys (action-insert [:a :b] {:foo 1})))))
    (is (= #{:assoc-in [:a :b] {:foo 1}}
           (into #{}(vals (action-insert [:a :b] {:foo 1})))))
    (is (= :assoc-in
           (:action (action-insert [:a :b] {:foo 1}))))
    (is (= {:foo 1}
           (:v (action-insert [:a :b] {:foo 1}))))
    (is (= [:a :b]
           (:ks (action-insert [:a :b] {:foo 1}))))))


(defn action-send [system {:keys [to msg]}]
  (put! (:state-mgr system) [:msgs to] msg))

(defn pending-send-msgs [system to]
  (get! (:state-mgr system) [:msgs to]))

(def send-action-handlers
  {:send action-send})

(deftest handle-message-test
  (testing "the integration and handling of messages"
    (let [ehdlrs (merge
                   send-action-handlers
                   kvstore/action-handlers)
          state  (atom {})
          smgr   (kvstore/create state)
          system {:state-mgr smgr
                  :effect-handlers ehdlrs}]


     ;; initial state tests
      (is (=  "pub status is not currently available."
              (<!! (handle-message
                     system
                     "test-user"
                     "open pub"))))
      (is (= "pub status is not currently available."
             (<!! (pending-send-msgs system "test-user"))))
      (is (= "No information available on line length in the specified area."
              (<!! (handle-message
                     system
                     "test-user"
                     "shortest"))))
      (is (= "Sorry, there is no information available on line length in the specified area."
             (<!! (pending-send-msgs system "test-user"))))
      (is (= "No information on the length of the given line"
              (<!! (handle-message
                     system
                     "test-user"
                     "length pub"))))
      (is (= "Sorry, there is no information available on your requested line."
             (<!! (pending-send-msgs system "test-user"))))
      (is (=   "No information available on line lengths."
              (<!! (handle-message
                     system
                     "test-user"
                     "under 20"))))
      (is (= "Sorry, there is no information available on line lengths."
             (<!! (pending-send-msgs system "test-user"))))



       ;; update-line-length tests
       ;;
       ;; invalid input tests
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update"))))
      (is (= "Invalid input. Please try again using the format 'update number name'"
            (<!! (pending-send-msgs system "test-user"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update cat pub"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 6 cat"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 5"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 37 83"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 2.5 bowls"))))
      (is (= "Invalid input to update-line-length."
             (<!! (handle-message
                    system
                    "test-user"
                    "update -5 bowls"))))
      ;; valid input tests
      (is (= "Wait time in the bowls line successfully updated."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 10 bowls"))))
     (is (= "Thank you for updating the length of the bowls line."
           (<!! (pending-send-msgs system "test-user"))))
     (is (= "No information on the length of the given line"
             (<!! (handle-message
                    system
                    "test-user"
                    "length pub"))))
     (is (= "Sorry, there is no information available on your requested line."
            (<!! (pending-send-msgs system "test-user")))
      (is (= "Wait time in the randwich line successfully updated."
             (<!! (handle-message
                    system
                    "test-user"
                    "update 7 randwich")))))
     (is (= "No information available on line length in the specified area."
             (<!! (handle-message
                    system
                    "test-user"
                    "shortest other"))))
     (is (= "Sorry, there is no information available on line length in the specified area."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Wait time in the pasta line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 30 pasta"))))
     (is (= "Wait time in the pub line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 14 pub"))))
     (is (= "Wait time in the grins line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 10 grins"))))
     (is (= "Wait time in the grins line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 16 grins"))))
     (is (= "Wait time in the kissam line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 7 kissam"))))


    ;; update-open-status tests
    ;;
    ;; invalid input tests
     (is (= "Invalid input to update-open-status."
            (<!! (handle-message
                   system
                   "test-user"
                   "status"))))
     (is (= "Invalid input. Please try again using the format 'status name open/closed'"
           (<!! (pending-send-msgs system "test-user"))))
     (is (= "Invalid input to update-open-status."
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins pink"))))
     (is (= "Invalid input to update-open-status."
            (<!! (handle-message
                   system
                   "test-user"
                   "status pink open"))))
     (is (= "Invalid input to update-open-status."
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins"))))
     ;; valid input tests
     (is (= "grins is already marked as open."
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins open")))
      (is (= "Thank you for reporting grins is still open."
            (<!! (pending-send-msgs system "test-user")))))
     (is (= "grins is now marked as closed :("
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins closed")))
      (is (= "Thank you for reporting grins has closed."
            (<!! (pending-send-msgs system "test-user")))))
     (is (= "grins is already marked as closed."
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins closed")))
      (is (= "Thank you for reporting grins is still closed."
            (<!! (pending-send-msgs system "test-user")))))
     (is (= "grins is now marked as open!"
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins open")))
      (is (= "Thank you for reporting grins has opened."
            (<!! (pending-send-msgs system "test-user")))))

     ;; get-open-status tests
     ;; invalid input tests
     (is (= "Invalid line name given to get-open-status"
            (<!! (handle-message
                   system
                   "test-user"
                   "open"))))
     (is (= "Invalid input. Please try again using the format 'open name'"
           (<!! (pending-send-msgs system "test-user"))))
     (is (= "Invalid line name given to get-open-status"
            (<!! (handle-message
                   system
                   "test-user"
                   "open pink"))))
     ;; valid input tests
     (is (= "Texting the user that grins is open."
            (<!! (handle-message
                   system
                   "test-user"
                   "open grins"))))
     (is (= "grins is open."
           (<!! (pending-send-msgs system "test-user"))))
     (is (= "grins is now marked as closed :("
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins closed"))))
     (is (= "Texting the user that grins is closed."
            (<!! (handle-message
                   system
                   "test-user"
                   "open grins"))))
     (is (= "grins is closed."
           (<!! (pending-send-msgs system "test-user"))))

     ;; shortest-line tests
     (is (= "Texting the user the name of shortest line on campus."
            (<!! (handle-message
                   system
                   "test-user"
                   "shortest pink"))))
     (is (= "The shortest line on campus is the kissam line."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texting the user the name of shortest line on campus."
            (<!! (handle-message
                   system
                   "test-user"
                   "shortest"))))
     (is (= "The shortest line on campus is the kissam line."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texting the user the name of shortest line in rand."
            (<!! (handle-message
                   system
                   "test-user"
                   "shortest rand"))))
     (is (= "The shortest line in rand is the randwich line."
            (<!! (pending-send-msgs system "test-user"))))

     ;; length-line tests
     ;; invalid input
     (is (= "Invalid line name sent to length-line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length"))))
     (is (= "Please try again with the name of a valid campus dining location."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Invalid line name sent to length-line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length purple"))))
     (is (= "Please try again with the name of a valid campus dining location."
            (<!! (pending-send-msgs system "test-user"))))
     ;; valid input
     (is (= "Wait time in the grins line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 16 grins"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length bowls"))))
     (is (= "The bowls line is currently 20 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length randwich"))))
     (is (= "The randwich line is currently 16 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length pasta"))))
     (is (= "The pasta line is currently 45 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length pub"))))
     (is (= "The pub line is currently 39 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length grins"))))
     (is (= "The grins line is currently 56 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texted the user the length of the specified line."
            (<!! (handle-message
                   system
                   "test-user"
                   "length kissam"))))
     (is (= "The kissam line is currently 12 minutes long."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "grins is now marked as closed :("
            (<!! (handle-message
                   system
                   "test-user"
                   "status grins closed"))))
     (is (= "Texted the user that the line is closed."
            (<!! (handle-message
                   system
                   "test-user"
                   "length grins"))))
     (is (= "grins is currently closed."
            (<!! (pending-send-msgs system "test-user"))))


     ;; lines-under-length tests
     ;; invalid input
     (is (= "Invalid time limit sent to lines-under-length."
            (<!! (handle-message
                  system
                  "test-user"
                  "under"))))
     (is (= "Please try again with a valid time limit."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Invalid time limit sent to lines-under-length."
            (<!! (handle-message
                  system
                  "test-user"
                  "under -10"))))
     (is (= "Invalid time limit sent to lines-under-length."
            (<!! (handle-message
                  system
                  "test-user"
                  "under 0"))))
     (is (= "Invalid time limit sent to lines-under-length."
            (<!! (handle-message
                  system
                  "test-user"
                  "under pink"))))
     (is (= "Invalid time limit sent to lines-under-length."
            (<!! (handle-message
                  system
                  "test-user"
                  "under 3.4"))))
     (is (= "Please try again with a valid time limit."
            (<!! (pending-send-msgs system "test-user"))))
     ;; valid input
     (is (= "Wait time in the grins line successfully updated."
            (<!! (handle-message
                   system
                   "test-user"
                   "update 16 grins"))))
     (is (= "Texting the user that no lines are under the limit."
            (<!! (handle-message
                  system
                  "test-user"
                  "under 12"))))
     (is (= "There are currently no lines under a 12 minute wait."
            (<!! (pending-send-msgs system "test-user"))))
     (is (= "Texting the user the list of lines under 18 minutes."
            (<!! (handle-message
                  system
                  "test-user"
                  "under 18"))))
     (is (or (= "The lines under a 18 minute wait are randwich, kissam"
                (<!! (pending-send-msgs system "test-user")))
             (= "The lines under a 18 minute wait are kissam, randwich"
                    (<!! (pending-send-msgs system "test-user"))))))))
