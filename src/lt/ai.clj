(ns lt.ai
  (:require
   [clj-http.client :as client]
   [clojure.string :as s]
   [clojure.tools.logging :as log]))

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

(defn generate-completion [conversation opts]
  (let [request (-> (chat-form conversation opts)
                    (wrap-request opts))
        _ (log/debug (str ">>ai " request))
        response (client/post (url "chat" "completions")
                              request)
        _ (log/debug (str "<<ai " response))]
    (conj conversation
          (-> response
              (get-in [:body :choices 0 :message])
              (update :role keyword)))))
