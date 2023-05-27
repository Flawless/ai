(ns lt.ai
  (:require
   [clj-http.client :as client]
   [clojure.string :as s]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]))

(defn- url [& path]
  (s/join "/" (into ["https://api.openai.com" "v1"] path)))

(def chat-models {
                  ;; default choice
                  :gpt-3.5 "gpt-3.5-turbo"
                  ;; fast and chepest
                  :ada "text-ada-001"})

(def default-chat-model (chat-models :gpt-3.5))

(defn- chat-form [conversation {:keys [max-tokens temperature model]
                               :or {max-tokens 2048
                                    temperature 0
                                    model default-chat-model}}]
  {:model model
   :messages conversation
   :max_tokens max-tokens
   :temperature temperature})

(defn- gen-bearer [auth]
  (str "Bearer " auth))

(defn- wrap-request
  ([{:keys [^String auth]}]
   {:headers {"Authorization" (gen-bearer auth)}
    :as :json
    :coerce :always
    :content-type :json})

  ([form {:keys [^String auth]}]
   {:headers {"Authorization" (gen-bearer auth)}
    :as :json
    :coerce :always
    :content-type :json
    :form-params form}))

(defn- send-request [request {:keys [retries timeout auto-timeout?]
                              :or {retries 0
                                   timeout (long (rand 1000))
                                   auto-timeout? true}
                              :as opts}]
  (try+
   (client/post (url "chat" "completions")
                request)
   (catch [:status 429] {:keys [error]}
     (if (pos? retries)
       (do
         (log/warnf "Got AI error: %s, retrying in %d ms" error timeout)
         (Thread/sleep timeout)
         (send-request request (-> opts
                                   (update :retries dec)
                                   (cond-> auto-timeout?
                                     ;; don't update, cuz timeout is nillable
                                     (assoc :timeout (* timeout 2))))))
       (throw+)))))

(defn generate-completion-async [conversation opts]
  (future
    (let [request (-> (chat-form conversation opts)
                      (wrap-request opts))
          _ (log/debug (str ">>ai " request))
          response (send-request request opts)
          _ (log/debug (str "<<ai " response))]
      (conj conversation
            (-> response
                (get-in [:body :choices 0 :message])
                (update :role keyword))))))

(defn generate-completion [conversation opts]
  @(generate-completion-async conversation opts))
