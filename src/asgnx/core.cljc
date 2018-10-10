(ns asgnx.core
  (:require [clojure.string :as string]
            [clojure.core.async :as async :refer [go chan <! >!]]
            [asgnx.kvstore :as kvstore
             :refer [put! get! list! remove!]]))

;; This is a helper function that you might want to use to implement
;; `cmd` and `args`.
(defn words [msg]
  (if msg
      (string/split msg #" ")
      []))

;; Map that stores the average amount of time it takes for one person to go
;; through each campus dining line in minutes and the general area of each location
(def dining-info {"bowls"     {:time 2 :area "rand"}
                  "randwich"  {:time 2.25 :area "rand"}
                  "pasta"     {:time 1.5 :area "rand"}
                  "pub"       {:time 2.75 :area "other"}
                  "grins"     {:time 3.5 :area "other"}
                  "kissam"    {:time 1.75 :area "other"}})

;; Do not edit!
;; A def for the course home page URL.
(def cs4278-brightspace "https://brightspace.vanderbilt.edu/d2l/home/85892")

;; Asgn 1.
;;
;; @Todo: Fill in this function to return the first word in a text
;; message.
;;
;; Example: (cmd "foo bar") => "foo"
;;
;; See the cmd-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn cmd [msg]
  (first (words msg)))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return the list of words following
;; the command in a text message.
;;
;; Example: (args "foo bar baz") => ("bar" "baz")
;;
;; See the args-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn args [msg]
  (rest (words msg)))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return a map with keys for the
;; :cmd and :args parsed from the msg.
;;
;; Example:
;;
;; (parsed-msg "foo bar baz") => {:cmd "foo" :args ["bar" "baz"]}
;;
;; See the parsed-msg-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn parsed-msg [msg]
  (hash-map :cmd (cmd msg) :args (args msg)))


;; Asgn 1.
;;
;; @Todo: Fill in this function to prefix the first of the args
;; in a parsed message with "Welcome " and return the result.
;;
;; Example:
;;
;; (welcome {:cmd "welcome" :args ["foo"]}) => "Welcome foo"
;;
;; See the welcome-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn welcome [pmsg]
  (let [x (str "Welcome " (first (get pmsg :args)))]
    (do (print x) x)))

;; Asgn 1.
;;
;; @Todo: Fill in this function to return the CS 4278 home page.
;; Use the `cs4278-brightspace` def to produce the output.
;;
;; See the homepage-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn homepage [_]
  cs4278-brightspace)


;; Asgn 2.
;;
;; @Todo: Create a function called action-send-msg that takes
;; a destination for the msg in a parameter called `to`
;; and the message in a parameter called `msg` and returns
;; a map with the keys :to and :msg bound to each parameter.
;; The map should also have the key :action bound to the value
;; :send.
;;
(defn action-send-msg [to msg]
  (hash-map :to to :msg msg :action :send))

;; Asgn 2.
;;
;; @Todo: Create a function called action-insert that takes
;; a list of keys in a `ks` parameter, a value to bind to that
;; key path to in a `v` parameter, and returns a map with
;; the key :ks bound to the `ks` parameter value and the key :v
;; vound to the `v` parameter value.)
;; The map should also have the key :action bound to the value
;; :assoc-in.
;;
(defn action-insert [ks v]
  (sorted-map :action :assoc-in :ks ks :v v))


;; update-line-length
;;
;; Updates the length of a specified campus dining line by accepting the
;; number of people currently in line and multiplying by the average time per person.
;; Does not directly change the application state, but returns list of side effects
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "update"
;;     :user-id "+15555555555"
;;     :args ["number" "line name"]
;; where "number" contains the number of people currently in the line "line name"
(defn update-line-length [ line-lengths {:keys [user-id args]}]
  (let [number (if (not (nil? (first args)))
                 (read-string (first args))
                 "invalid")
        line (second args)]
   ;; If input is valid, store the newly calculated wait time (to the closest whole minute)
   (if
     (and (integer? number) (>= number 0) (not (nil? (get dining-info line))))
    [[(action-insert [:lengths] (assoc line-lengths line (int (+ 0.5 (* number (get-in dining-info [line :time]))))))
      (action-send-msg user-id (str "Thank you for updating the length of the " line " line."))]
     (str "Wait time in the " line " line successfully updated.")]
    [[(action-send-msg user-id "Invalid input. Please try again using the format 'update number name'")]
     "Invalid input to update-line-length."])))


;; update-open-status
;;
;; Updates the status of the specified dining location, marking it
;; as either open or closed
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "status"
;;     :user-id "+15555555555"
;;     :args ["line name" open/closed")]]])))
;; where "line name" is the name of a campus dining location
;; and "open/closed" is either the string "open" or "closed
(defn update-open-status [line-lengths {:keys [user-id args]}]
  (let [line (first args)
        status (second args)
        curr-status (get line-lengths line)]
   (cond
     ;; Invalid input
     (or (not (or (= status "open") (= status "closed"))) (nil? (get dining-info line)))
     [[(action-send-msg user-id "Invalid input. Please try again using the format 'status name open/closed'")]
      "Invalid input to update-open-status."]
     ;; From closed to open
     (and (or (nil? curr-status)(= -1 curr-status))(= "open" status))
     [[(action-insert [:lengths] (assoc line-lengths line 0))
       (action-send-msg user-id (str "Thank you for reporting " line " has opened."))]
      (str line " is now marked as open!")]
     ;; From open to closed
     (and (not (= -1 curr-status)) (= "closed" status))
     [[(action-insert [:lengths] (assoc line-lengths line -1))
       (action-send-msg user-id (str "Thank you for reporting " line " has closed."))]
      (str line " is now marked as closed :(")]
     ;; No status change
     :else
     [[(action-send-msg user-id (str "Thank you for reporting " line " is still " status "."))]
      (str line " is already marked as " status ".")])))


;; get-open-status
;;
;; Responds to the user with a text indicating whether the specified dining
;; location is open or closed
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "open"
;;     :user-id "+15555555555"
;;     :args ["line name"]
(defn get-open-status [line-lengths {:keys [args user-id]}]
  (let [line (first args)
        status (get line-lengths line)
        answer (if (= -1 status)
                 "closed"
                 "open")]
    (cond
      ;; Invalid line name
      (nil? (get dining-info line))
      [[(action-send-msg user-id "Invalid input. Please try again using the format 'open name'")]
       "Invalid line name given to get-open-status"]
      ;; No information on open status
      (nil? status)
      [[(action-send-msg user-id (str line " status is not currently available."))]
       (str line " status is not currently available.")]
      ;; Valid line name with information--text the user
      :else
      [[(action-send-msg user-id (str line " is " answer "."))]
       (str "Texting the user that " line " is " answer ".")])))


;; shortest-line
;;
;; Responds to the user with a text indicating the campus dining line that is the shortest
;; The user may also specify if they want to know the shortest line within a certain area
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "shortest"
;;     :user-id "+15555555555"
;;     :args ["area"]
;; where "area" is the name of an area of campus dining (ex. "rand")
;; The "area" paramter is optional
(defn shortest-line [line-lengths {:keys [user-id args]}]
  (let [area (first args)
        lengths-in-area (if (or (= area "rand") (= area "other"))
                          (filter #(= area (get-in dining-info [(first %) :area])) line-lengths)
                          line-lengths)
        format-area (if (or (= area "rand") (= area "other"))
                      (str "in " area)
                      "on campus")]

    (cond
     ;; If there is info available for desired areas
     (not (or (empty? lengths-in-area) (nil? lengths-in-area)))
     [[(action-send-msg user-id (str "The shortest line " format-area " is the "
                                    ;; @InspiredBy
                                    ;; @Source: https://www.spacjer.com/blog/2016/01/12/lesser-known-clojure-max-key-and-min-key/
                                     (first (apply min-key second (filter #(not (= (second %) -1)) lengths-in-area)))
                                     ;; @EndInspiredBy
                                     " line."))]
      (str "Texting the user the name of shortest line " format-area ".")]

     ;; No information available on specified areas
     :else
     [[(action-send-msg user-id "Sorry, there is no information available on line length in the specified area.")]
      "No information available on line length in the specified area."])))


;; length-line
;;
;; Responds to the user with a text indicating the length of a given line
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "length"
;;     :user-id "+15555555555"
;;     :args ["line name"]
;; where "line name" is the name of a campus dining location
(defn length-line [line-lengths {:keys [user-id args]}]
  (let [line (first args)
        length (get line-lengths line)]
    (cond
      ;; Invalid line name given
      (nil? (get dining-info line))
      [[(action-send-msg user-id "Please try again with the name of a valid campus dining location.")]
       "Invalid line name sent to length-line."]
      ;; No information on specified line
      (nil? length)
      [[(action-send-msg user-id "Sorry, there is no information available on your requested line.")]
       "No information on the length of the given line"]
      ;; The specifed dining location is closed
      (= -1 length)
      [[(action-send-msg user-id (str line " is currently closed."))] "Texted the user that the line is closed."]
      ;; The dining location has information store and is open
      :else
      [[(action-send-msg user-id (str "The " line " line is currently " length " minutes long."))]
       "Texted the user the length of the specified line."])))


;; lines-under-length
;;
;; Responds to the user with a text of the list of campus dining locations
;; with lines currently under the specified limit
;;
;; Takes input as a parsed message with the format:
;;    {:cmd "under"
;;     :user-id "+15555555555"
;;     :args ["minutes"]
;; where minutes is the specified time limit lines the user is interested in
(defn lines-under-length [line-lengths {:keys [user-id args]}]
  (let [time-limit (if (nil? (first args))
                     "invalid"
                     (read-string (first args)))]
    (cond
      ;; Invalid time limit
      (or (not (integer? time-limit)) (<= time-limit 0))
      [[(action-send-msg user-id "Please try again with a valid time limit.")]
       "Invalid time limit sent to lines-under-length."]
      ;; No line length info entered
      (nil? line-lengths)
      [[(action-send-msg user-id "Sorry, there is no information available on line lengths.")]
       "No information available on line lengths."]
      ;; Valid input--find lines under the given time limit
      :else
      (let [lines-under (filter
                         #(and (not (= (second %) -1))(< (second %) time-limit)) line-lengths)]
        (if (empty? lines-under)
         ;; No lines under time limit
          [[(action-send-msg user-id (str "There are currently no lines under a " time-limit " minute wait."))]
           "Texting the user that no lines are under the limit."]
         ;; Send the user the list of lines under the time limit
          [[(action-send-msg user-id (str "The lines under a " time-limit " minute wait are "
                                      (string/join ", " (reduce #(conj %1 (first %2)) [] lines-under))))]
           (str "Texting the user the list of lines under " time-limit " minutes.")])))))


;; Don't edit!
(defn stateless [f]
  (fn [_ & args]
    [[] (apply f args)]))


(def routes {"default"  (stateless (fn [& args] "Unknown command."))
             "welcome"  (stateless welcome)
             "homepage" (stateless homepage)
             "update"   update-line-length
             "status"   update-open-status
             "open"     get-open-status
             "shortest" shortest-line
             "length"   length-line
             "under"    lines-under-length})


(defn line-lengths-query [state-mgr pmsg]
  (get! state-mgr [:lengths]))


(def queries
  {"update"   line-lengths-query
   "status"   line-lengths-query
   "open"     line-lengths-query
   "shortest" line-lengths-query
   "length"   line-lengths-query
   "under"    line-lengths-query})


;; Don't edit!
(defn read-state [state-mgr pmsg]
  (go
    (if-let [qfn (get queries (:cmd pmsg))]
      (<! (qfn state-mgr pmsg))
      {})))


;; Asgn 1.
;;
;; @Todo: This function should return a function (<== pay attention to the
;; return type) that takes a parsed message as input and returns the
;; function in the `routes` map that is associated with a key matching
;; the `:cmd` in the parsed message. The returned function would return
;; `welcome` if invoked with `{:cmd "welcome"}`.
;;
;; Example:
;;
;; (let [msg {:cmd "welcome" :args ["bob"]}]
;;   (((create-router {"welcome" welcome}) msg) msg) => "Welcome bob"
;;
;; If there isn't a function in the routes map that is mapped to a
;; corresponding key for the command, you should return the function
;; mapped to the key "default".
;;
;; See the create-router-test in test/asgnx/core_test.clj for the
;; complete specification.
;;
(defn create-router [routes]
  (fn [pmsg] (let [cmd (get pmsg :cmd)]
               (if (nil? (get routes cmd))
                 (get routes "default")
                 (get routes cmd)))))


;; Don't edit!
(defn output [o]
  (second o))


;; Don't edit!
(defn actions [o]
  (first o))


;; Don't edit!
(defn invoke [{:keys [effect-handlers] :as system} e]
  (go
    (println "    Invoke:" e)
    (if-let [action (get effect-handlers (:action e))]
      (do
        (println "    Invoking:" action "with" e)
        (<! (action system e))))))


;; Don't edit!
(defn process-actions [system actions]
  (go
    (println "  Processing actions:" actions)
    (let [results (atom [])]
      (doseq [action actions]
        (let [result (<! (invoke system action))]
          (swap! results conj result)))
      @results)))


;; Don't edit!
(defn handle-message
  "
    This function orchestrates the processing of incoming messages
    and glues all of the pieces of the processing pipeline together.

    The basic flow to handle a message is as follows:

    1. Create the router that will be used later to find the
       function to handle the message
    2. Parse the message
    3. Load any saved state that is going to be needed to process
       the message (e.g., querying the list of experts, etc.)
    4. Find the function that can handle the message
    5. Call the handler function with the state from #3 and
       the message
    6. Run the different actions that the handler returned...these actions
       will be bound to different implementations depending on the environemnt
       (e.g., in test, the actions aren't going to send real text messages)
    7. Return the string response to the message

  "
  [{:keys [state-mgr] :as system} src msg]
  (go
    (println "=========================================")
    (println "  Processing:\"" msg "\" from" src)
    (let [rtr    (create-router routes)
          _      (println "  Router:" rtr)
          pmsg   (assoc (parsed-msg msg) :user-id src)
          _      (println "  Parsed msg:" pmsg)
          state  (<! (read-state state-mgr pmsg))
          _      (println "  Read state:" state)
          hdlr   (rtr pmsg)
          _      (println "  Hdlr:" hdlr)
          [as o] (hdlr state pmsg)
          _      (println "  Hdlr result:" [as o])
          arslt  (<! (process-actions system as))
          _      (println "  Action results:" arslt)]
      (println "=========================================")
      o)))
