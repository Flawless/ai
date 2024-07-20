(ns lt.ai
  (:require
   [clj-http.client :as client]
   [clojure.string :as s]
   [clojure.tools.logging :as log]
   [slingshot.slingshot :refer [throw+ try+]]))

(defn- url [& path]
  (s/join "/" (into ["https://api.openai.com" "v1"] path)))

(def chat-models {;; default choice
                  :gpt-3.5 "gpt-3.5-turbo"
                  ;; fast and chepest
                  :ada "text-ada-001"})

(def default-chat-model (chat-models :gpt-3.5))

(defn- chat-form [conversation {:keys [max-tokens temperature model extra]
                                :or {max-tokens 2048
                                     temperature 0
                                     model default-chat-model}}]
  (merge extra
         {:model model
          :messages conversation
          :max_tokens max-tokens
          :temperature temperature}))

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
   (catch [:status 429] e
     (if (pos? retries)
       (do
         (log/warnf "Got AI error: %s, retrying in %d ms" (-> e :body :error :message) timeout)
         (Thread/sleep timeout)
         (send-request request (-> opts
                                   (update :retries dec)
                                   (cond-> auto-timeout?
                                     ;; don't update, cuz timeout is nillable
                                     (assoc :timeout (+ (* timeout 2) (rand-int 1000)))))))
       (throw+)))))

(defn generate-completion-async [conversation opts]
  (future
    (let [request (-> (chat-form conversation opts)
                      (wrap-request opts))
          _ (log/debug (str ">>ai " request))
          {:keys [body] :as response} (send-request request opts)
          _ (log/debug (str "<<ai " response))]
      {:conversation (conj conversation
                           (-> body
                               (get-in [:choices 0 :message])
                               (update :role keyword)))
       :usage {:prompt-tokens (-> body :usage :prompt_tokens)
               :completion-tokens (-> body :usage :completion_tokens)
               :total-tokens (-> body :usage :total_tokens)}})))

(defn generate-completion [conversation opts]
  @(generate-completion-async conversation opts))

(comment
  (dotimes [x 4]
    (generate-completion [{:role :system :content (str "You are translator bot and should translate each user message to desired language (English if not specified otherwise).")}
                          {:role :user :content (format "Please, introduce yourself as an translation bot in language defined by locale \"%s\" and give a brief instruction. Also note, that you can process voice messages along with texts." "russian")}]
                         {:auth "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"
                          :retries 3})
    (println x))

  ;;
  )
