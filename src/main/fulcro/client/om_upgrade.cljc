(ns fulcro.client.om-upgrade
  (:require [om.next :as om]
            [om.next.protocols :as p]
            [om.next.impl.parser :as parser]
            [om.util :as util]
    #?@(:cljs [[om.next.cache :as c]
               [goog.object :as gobj]])
            [clojure.pprint :refer [pprint]]
            [fulcro.client.logging :as log]
            [fulcro.client.util :as futil]
            [clojure.walk :refer [prewalk]]
            [clojure.string :as str]))

(defn query-id
  "Returns a string ID for the query of the given class with qualifier"
  [class qualifier]
  (let [classname #?(:clj (-> (str (-> class meta :component-ns) "." (-> class meta :component-name))
                            (str/replace "." "$")
                            (str/replace "-" "_"))
                     :cljs (.-name class))]
    (str classname (when qualifier (str "$" qualifier)))))

#?(:clj
   (defn init-local-state [component]
     (reset! (:state component) (.initLocalState component))))

#?(:clj
   (defn factory
     "Create a factory constructor from a component class created with
      om.next/defui."
     ([class]
      (factory class nil))
     ([class {:keys [validator keyfn instrument? qualifier]
              :or   {instrument? true} :as opts}]
      {:pre [(fn? class)]}
      (with-meta
        (fn self
          ([] (self nil))
          ([props & children]
           (when-not (nil? validator)
             (assert (validator props)))
           (if (and (var-get #'om/*instrument*) instrument?)
             (#'om/*instrument*
               {:props    props
                :children children
                :class    class
                :factory  (factory class (assoc opts :instrument? false))})
             (let [react-key (cond
                               (some? keyfn) (keyfn props)
                               (some? (:react-key props)) (:react-key props)
                               :else (#'om/compute-react-key class props))
                   ctor      class
                   ref       (:ref props)
                   props     {:omcljs$reactRef   ref
                              :omcljs$reactKey   react-key
                              :omcljs$value      (cond-> props
                                                   (map? props) (dissoc :ref))
                              :omcljs$queryid    (query-id class qualifier)
                              :omcljs$mounted?   (atom false)
                              :omcljs$path       (-> props meta :om-path)
                              :omcljs$reconciler #'om/*reconciler*
                              :omcljs$parent     #'om/*parent*
                              :omcljs$shared     #'om/*shared*
                              :omcljs$instrument #'om/*instrument*
                              :omcljs$depth      #'om/*depth*}
                   component (ctor (atom nil) (atom nil) props children)]
               (when ref
                 (assert (some? #'om/*parent*))
                 (swap! (:refs #'om/*parent*) assoc ref component))
               (init-local-state component)
               component))))
        {:class     class
         :queryid   (query-id class qualifier)
         :qualifier qualifier}))))

#?(:cljs
   (defn create-element [class props children]
     (js/React.createElement class props children)))

#?(:cljs
   (defn factory
     "Create a factory constructor from a component class created with
      om.next/defui."
     ([class] (factory class nil))
     ([class {:keys [validator keyfn instrument? qualifier]
              :or   {instrument? true} :as opts}]
      {:pre [(fn? class)]}
      (with-meta
        (fn self [props & children]
          (when-not (nil? validator)
            (assert (validator props)))
          (if (and om/*instrument* instrument?)
            (om/*instrument*
              {:props    props
               :children children
               :class    class
               :factory  (factory class (assoc opts :instrument? false))})
            (let [key (if-not (nil? keyfn)
                        (keyfn props)
                        (om/compute-react-key class props))
                  ref (:ref props)
                  ref (cond-> ref (keyword? ref) str)
                  t   (if-not (nil? om/*reconciler*)
                        (p/basis-t om/*reconciler*)
                        0)]
              (create-element class
                #js {:key               key
                     :ref               ref
                     :omcljs$reactKey   key
                     :omcljs$value      (om/om-props props t)
                     :omcljs$path       (-> props meta :om-path)
                     :omcljs$queryid    (query-id class qualifier)
                     :omcljs$reconciler om/*reconciler*
                     :omcljs$parent     om/*parent*
                     :omcljs$shared     om/*shared*
                     :omcljs$instrument om/*instrument*
                     :omcljs$depth      om/*depth*}
                (util/force-children children)))))
        {:class     class
         :queryid   (query-id class qualifier)
         :qualifier qualifier}))))

(defn denormalize-query
  "Takes a state map that may contain normalized queries and a query ID. Returns the stored query or nil."
  [state-map ID]
  (let [get-stored-query (fn [id] (get-in state-map [::queries id :query]))]
    (when-let [normalized-query (get-stored-query ID)]
      (prewalk (fn [ele]
                 (if-let [q (and (string? ele) (get-stored-query ele))]
                   q
                   ele)) normalized-query))))

(defn get-query-by-id [state-map class queryid]
  (let [static-query #?(:clj  ((-> class meta :query) class)
                        :cljs (om/query class))
        static-params #?(:clj ((-> class meta :params) class)
                         :cljs (om/params class))
        query                 (or (denormalize-query state-map queryid) static-query)
        params                static-params
        c                     (-> static-query meta :component)]
    (assert (nil? c) (str "Query violation, " class, " reuses " c " query"))
    (with-meta (#'om/bind-query query params) {:component class
                                               :queryid   queryid})))

(defn get-query*
  ([class]
   (let [q #?(:clj      ((-> class meta :query) class)
              :cljs (om/query class))
         params #?(:clj ((-> class meta :params) class)
                   :cljs (om/params class))
         c              (-> q meta :component)]
     (assert (nil? c) (str "Query violation, " class, " reuses " c " query"))
     (with-meta (#'om/bind-query q params) {:component class
                                            :queryid   (query-id class nil)})))
  ([state-map factory]
   (assert (not (nil? (-> factory meta :class))) "get-query key must be an extended Om factory")
   (let [class     (-> factory meta :class)
         qualifier (-> factory meta :qualifier)
         queryid   (query-id class qualifier)]
     (get-query-by-id state-map class queryid))))

(declare normalize-query)

(defn link-element [element]
  (prewalk (fn [ele]
             (if-let [{:keys [queryid]} (meta ele)]
               queryid
               ele)) element))

(defn normalize-query-elements
  "Determines if there are query elements in the present query that need to be normalized as well. If so, it does so.
  Returns the new state map."
  [state-map query]
  (reduce (fn [state ele]
            (let [parameterized? (list? ele)
                  raw-element    (if parameterized? (first ele) ele)]
              (cond
                (util/union? raw-element) (let [union-alternates            (first (vals raw-element))
                                                normalized-union-alternates (into {} (map link-element union-alternates))
                                                union-query-id              (-> union-alternates meta :queryid)]
                                            (assert union-query-id "Union query has an ID. Did you use extended get-query?")
                                            (futil/deep-merge
                                              {::queries {union-query-id {:query normalized-union-alternates
                                                                          :id    union-query-id}}}
                                              (reduce (fn [s [_ subquery]]
                                                        (normalize-query s subquery)) state union-alternates)))
                (util/join? raw-element) (normalize-query state (util/join-value raw-element))
                :else state)))
    state-map query))

(defn link-query
  "Find all of the elements (only at the top level) of the given query and replace them
  with their query ID"
  [query]
  (mapv link-element query))

(defn normalize-query
  "Given a state map and a query, returns a state map with the query normalized into the database. Query fragments
  that already appear in the state will not be added. "
  [state-map query]
  (let [new-state (normalize-query-elements state-map query)
        top-query (link-query query)]
    (if-let [{:keys [queryid]} (meta query)]
      (futil/deep-merge {::queries {queryid {:query top-query
                                             :id    queryid}}} new-state)
      new-state)))

(defn set-query* [state-map class-or-ui-factory {:keys [query params]}]
  (if-let [queryid (if (contains? (meta class-or-ui-factory) :queryid)
                     (some-> class-or-ui-factory meta :queryid)
                     (query-id class-or-ui-factory nil))]
    ; we have to dissoc the old one, because normalize won't overwrite by default
    (normalize-query (update state-map ::queries dissoc queryid) (with-meta query {:queryid queryid}))
    state-map))

(defn gather-keys
  "Gather the keys that would be considered part of the refresh set for the given query.

  E.g. [:a {:j [:b]} {:u {:x [:l] :y [:k]}}] ==> #{:a :j :u}"
  [query]
  (cond
    (vector? query) (reduce (fn [rv e]
                              (cond
                                (keyword? e) (conj rv e)
                                (and (list? e) (keyword? (first e))) (conj rv (first e))
                                (and (util/join? e) (keyword? (util/join-key e))) (conj rv (util/join-key e))
                                :else rv))
                      #{} query)
    (map? query) (-> query keys set)                        ; a union component, which has a map for a query
    :else #{}))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  #?(:clj  (get (:props c) k)
     :cljs (gobj/get (.-props c) k)))


(defn- path
  "Returns the component's Om data path."
  [c]
  (get-prop c #?(:clj  :omcljs$path
                 :cljs "omcljs$path")))


(defrecord Indexer [indexes]
  #?(:clj  clojure.lang.IDeref
     :cljs IDeref)
  #?(:clj  (deref [_] @indexes)
     :cljs (-deref [_] @indexes))

  p/IIndexer
  (index-root [this root-class]
    (assert (:state this) "State map is in `this` for indexing root")
    (let [prop->classes (atom {})
          state-map     (get this :state)
          rootq         (get-query* state-map (factory root-class nil))]
      (prewalk (fn [ele]
                 (when-let [component (some-> ele meta :component)]
                   (let [ks (gather-keys ele)]
                     (doseq [k ks]
                       (swap! prop->classes update k (fnil conj #{}) component))))
                 ele) rootq)
      (swap! indexes merge {:prop->classes @prop->classes})))

  (index-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes
                        [:class->components (om/react-type c)]
                        (fnil conj #{}) c)
              ident   (when #?(:clj  (satisfies? om/Ident c)
                               :cljs (implements? om/Ident c))
                        (let [ident (om/ident c (om/props c))]
                          (when-not (util/ident? ident)
                            (log/info
                              (str "malformed Ident. An ident must be a vector of "
                                "two elements (a keyword and an EDN value). Check "
                                "the Ident implementation of component `"
                                (.. c -constructor -displayName) "`.")))
                          (when-not (some? (second ident))
                            (log/info
                              (str "component " (.. c -constructor -displayName)
                                "'s ident (" ident ") has a `nil` second element."
                                " This warning can be safely ignored if that is intended.")))
                          ident))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] (fnil conj #{}) c))
            indexes)))))

  (drop-component! [_ c]
    (swap! indexes
      (fn [indexes]
        (let [indexes (update-in indexes [:class->components (om/react-type c)] disj c)
              ident   (when #?(:clj  (satisfies? om/Ident c)
                               :cljs (implements? om/Ident c))
                        (om/ident c (om/props c)))]
          (if-not (nil? ident)
            (cond-> indexes
              ident (update-in [:ref->components ident] disj c))
            indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (om/component? k)
        #{k}
        (transduce (map #(get-in indexes [:class->components %]))
          (completing into)
          (get-in indexes [:ref->components k] #{})
          (get-in indexes [:prop->classes k]))))))

(defn fulcro-ui->props
  "Finds props for a given component. Returns ::no-ident if the component has
  no ident (which prevents localized update). This eliminates the need for
  path data."
  [{:keys [parser state #?(:clj pathopt :cljs ^boolean pathopt)] :as env} c]
  (let [ui (when (and pathopt #?(:clj  (satisfies? om/Ident c)
                                 :cljs (implements? om/Ident c)) (om/iquery? c))
             (let [id (om/ident c (om/props c))]
               (get (parser env [{id (om/get-query c)}]) id)))]
    (if ui
      ui
      ::no-ident)))

(defrecord Reconciler [config state]
  #?(:clj  clojure.lang.IDeref
     :cljs IDeref)
  #?(:clj  (deref [this] @(:state config))
     :cljs (-deref [_] @(:state config)))

  p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret          (atom nil)
          rctor        (factory root-class)
          guid #?(:clj (java.util.UUID/randomUUID)
                  :cljs (random-uuid))]
      (when (om/iquery? root-class)
        (p/index-root (assoc (:indexer config) :state (-> config :state deref)) root-class))
      (when (and (:normalize config)
              (not (:normalized @state)))
        (let [new-state (om/tree->db root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)))
      (let [renderf (fn [data]
                      (binding [om/*reconciler* this
                                om/*shared*     (merge
                                                  (:shared config)
                                                  (when (:shared-fn config)
                                                    ((:shared-fn config) data)))
                                om/*instrument* (:instrument config)]
                        (let [c (cond
                                  #?@(:cljs [(not (nil? target)) ((:root-render config) (rctor data) target)])
                                  (nil? @ret) (rctor data)
                                  :else (when-let [c' @ret]
                                          #?(:clj  (do
                                                     (reset! ret nil)
                                                     (rctor data))
                                             :cljs (when (om/mounted? c')
                                                     (.forceUpdate c' data)))))]
                          (when (and (nil? @ret) (not (nil? c)))
                            (swap! state assoc :root c)
                            (reset! ret c)))))
            parsef  (fn []
                      (let [sel (get-query* (-> config :state deref) rctor)]
                        (assert (or (nil? sel) (vector? sel))
                          "Application root query must be a vector")
                        (if-not (nil? sel)
                          (let [env (#'om/to-env config)
                                v   ((:parser config) env sel)]
                            (when-not (empty? v)
                              (renderf v)))
                          (renderf @(:state config)))))]
        (swap! state merge
          {:target target :render parsef :root root-class
           :remove (fn []
                     (remove-watch (:state config) (or target guid))
                     (swap! state
                       #(-> %
                          (dissoc :target) (dissoc :render) (dissoc :root)
                          (dissoc :remove)))
                     (when-not (nil? target)
                       ((:root-unmount config) target)))})
        (add-watch (:state config) (or target guid)
          (fn [_ _ _ _]
            (swap! state update-in [:t] inc)
            #?(:cljs
               (if-not (om/iquery? root-class)
                 (om/queue-render! parsef)
                 (om/schedule-render! this)))))
        (parsef)
        (when-let [sel (get-query* (-> config :state deref) rctor)]
          (let [env  (#'om/to-env config)
                snds (om/gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds
                  (fn send-cb
                    ([resp]
                     (om/merge! this resp nil)
                     (renderf ((:parser config) env sel)))
                    ([resp query]
                     (om/merge! this resp query)
                     (renderf ((:parser config) env sel)))
                    ([resp query remote]
                     (when-not (nil? remote)
                       (p/queue! this (keys resp) remote))
                     (om/merge! this resp query remote)
                     (p/reconcile! this remote))))))))
        @ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [this]
    (let [root (get @state :root)]
      (when (om/iquery? root)
        (let [indexer (:indexer config)
              c       (first (get-in @indexer [:class->components root]))]
          (p/index-root (assoc indexer :state (-> config :state deref)) (or c root))))))

  (queue! [this ks]
    (p/queue! this ks nil))
  (queue! [_ ks remote]
    (if-not (nil? remote)
      (swap! state update-in [:remote-queue remote] into ks)
      (swap! state update-in [:queue] into ks)))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
      (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (do
        (swap! state assoc :queued true)
        true)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc :sends-queued true)
        true)
      false))

  (reconcile! [this]
    (p/reconcile! this nil))
  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [this remote]
    (let [st @state
          q  (if-not (nil? remote)
               (get-in st [:remote-queue remote])
               (:queue st))]
      (swap! state update-in [:queued] not)
      (if (not (nil? remote))
        (swap! state assoc-in [:remote-queue remote] [])
        (swap! state assoc :queue []))
      "NOTES: I currently have elide-paths set in the parser via fulcro, and the incremental rendering turned off via this true:"
      (if true                                              ; (empty? q) 3ms average keypress overhead with path-opt optimizations and incremental
        ;; TODO: need to move root re-render logic outside of batching logic
        ((:render st))
        (let [cs   (transduce
                     (map #(p/key->components (:indexer config) %))
                     #(into %1 %2) #{} q)
              env  (#'om/to-env config)
              root (:root @state)]
          #?(:cljs
             (doseq [c ((:optimize config) cs)]             ; sort by depth
               (let [props-change? (> (p/basis-t this) (om/t c))]
                 (when (om/mounted? c)
                   (let [computed       (om/get-computed (om/props c))
                         next-raw-props (fulcro-ui->props env c)
                         force-root?    (= ::no-ident next-raw-props) ; screw focused query...
                         next-props     (when-not force-root? (om.next/computed next-raw-props computed))]
                     (if force-root?
                       (do
                         (log/debug "Re-render was forced from root because an out-of-date component had no ident: " (om/react-type c))
                         ; This will update our t on the rest of components so that the basis comparison above will short-circuit the rest
                         ((:render st)))
                       (do
                         (when (and (exists? (.-componentWillReceiveProps c))
                                 (om/iquery? root)
                                 props-change?)
                           (let [next-props (if (nil? next-props)
                                              (when-let [props (om/props c)]
                                                props)
                                              next-props)]
                             ;; `componentWilReceiveProps` is always called before `shouldComponentUpdate`
                             (.componentWillReceiveProps c
                               #js {:omcljs$value (om/om-props next-props (p/basis-t this))})))
                         (when (om/should-update? c next-props (om/get-state c))
                           (if-not (nil? next-props)
                             (om/update-component! c next-props)
                             (.forceUpdate c))
                           ;; Only applies if we're doing incremental rendering, not
                           ;; the case in applications without queries
                           (when (and (om/iquery? root)
                                   (not= c root)
                                   props-change?)
                             (when-let [update-path (path c)]
                               (loop [p (om/parent c)]
                                 (when (some? p)
                                   (let [update-path' (subvec update-path (count (path p)))]
                                     (om/update-props! p (assoc-in (om/props p) update-path' next-raw-props))
                                     (om/merge-pending-props! p)
                                     (recur (om/parent p)))))))))))))))))))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
          (fn [state]
            (-> state
              (assoc :queued-sends {})
              (assoc :sends-queued false))))
        ((:send config) sends
          (fn send-cb
            ([resp]
             (om/merge! this resp nil))
            ([resp query]
             (om/merge! this resp query))
            ([resp query remote]
             (when-not (nil? remote)
               (p/queue! this (keys resp) remote))
             (om/merge! this resp query remote)
             (p/reconcile! this remote))))))))



(defn reconciler
  "Construct a reconciler from a configuration map.

   Required parameters:
     :state        - the application state. If IAtom value is not supplied the
                     data will be normalized into the default database format
                     using the root query. This can be disabled by explicitly
                     setting the optional :normalize parameter to false.
     :parser       - the parser to be used

   Optional parameters:
     :shared       - a map of global shared properties for the component tree.
     :shared-fn    - a function to compute global shared properties from the root props.
                     the result is merged with :shared.
     :send         - required only if the parser will return a non-empty value when
                     run against the supplied :remotes. send is a function of two
                     arguments, the map of remote expressions keyed by remote target
                     and a callback which should be invoked with the result from each
                     remote target. Note this means the callback can be invoked
                     multiple times to support parallel fetching and incremental
                     loading if desired. The callback should take the response as the
                     first argument and the the query that was sent as the second
                     argument.
     :normalize    - whether the state should be normalized. If true it is assumed
                     all novelty introduced into the system will also need
                     normalization.
     :remotes      - a vector of keywords representing remote services which can
                     evaluate query expressions. Defaults to [:remote]
     :root-render  - the root render function. Defaults to ReactDOM.render
     :root-unmount - the root unmount function. Defaults to
                     ReactDOM.unmountComponentAtNode
     :logger       - supply a goog.log compatible logger
     :tx-listen    - a function of 2 arguments that will listen to transactions.
                     The first argument is the parser's env map also containing
                     the old and new state. The second argument is a map containing
                     the transaction, its result and the remote sends that the
                     transaction originated."
  [{:keys [state shared shared-fn
           parser normalize
           send merge-sends remotes
           merge merge-tree merge-ident
           prune-tree
           optimize
           history
           root-render root-unmount
           migrate id-key
           instrument tx-listen
           easy-reads]
    :or   {merge-sends  #(merge-with into %1 %2)
           remotes      [:remote]
           merge        om/default-merge
           merge-tree   om/default-merge-tree
           merge-ident  om/default-merge-ident
           prune-tree   om/default-extract-errors
           optimize     (fn [cs] (sort-by om/depth cs))
           history      100
           root-render  #?(:clj  (fn [c target] c)
                           :cljs #(js/ReactDOM.render %1 %2))
           root-unmount #?(:clj  (fn [x])
                           :cljs #(js/ReactDOM.unmountComponentAtNode %))
           migrate      om/default-migrate
           easy-reads   true}
    :as   config}]
  {:pre [(map? config)]}
  (let [idxr          (map->Indexer {:indexes (atom {})})
        norm? #?(:clj (instance? clojure.lang.Atom state)
                 :cljs (satisfies? IAtom state))
        state'        (if norm? state (atom state))
        logger        (if (contains? config :logger)
                        (:logger config)
                        #?(:cljs om/*logger*))
        ret           (Reconciler.
                        {:state       state' :shared shared :shared-fn shared-fn
                         :parser      parser :indexer idxr
                         :send        send :merge-sends merge-sends :remotes remotes
                         :merge       merge :merge-tree merge-tree :merge-ident merge-ident
                         :prune-tree  prune-tree
                         :optimize    optimize
                         :normalize   (or (not norm?) normalize)
                         :history     #?(:clj  []
                                         :cljs (c/cache history))
                         :root-render root-render :root-unmount root-unmount
                         :logger      logger :pathopt true
                         :migrate     migrate :id-key id-key
                         :instrument  instrument :tx-listen tx-listen
                         :easy-reads  easy-reads}
                        (atom {:queue        []
                               :remote-queue {}
                               :queued       false :queued-sends {}
                               :sends-queued false
                               :target       nil :root nil :render nil :remove nil
                               :t            0 :normalized norm?}))]
    ret))

(defn transact* [r c ref tx]
  (let [cfg        (:config r)
        ref        (if (and c #?(:clj  (satisfies? om/Ident c)
                                 :cljs (implements? om/Ident c)) (not ref))
                     (om/ident c (om/props c))
                     ref)
        env        (merge
                     (#'om/to-env cfg)
                     {:reconciler r :component c}
                     (when ref
                       {:ref ref}))
        id #?(:clj (java.util.UUID/randomUUID)
              :cljs (random-uuid))
        #?@(:cljs
            [_ (.add (:history cfg) id @(:state cfg))
             ])
        old-state  @(:state cfg)
        v          ((:parser cfg) env tx)
        snds       (om/gather-sends env tx (:remotes cfg))
        xs         (cond-> []
                     (not (nil? c)) (conj c)
                     (not (nil? ref)) (conj ref))]
    (p/queue! r (into xs (remove symbol?) (keys v)))
    (when-not (empty? snds)
      (doseq [[remote _] snds]
        (p/queue! r xs remote))
      (p/queue-sends! r snds)
      (om/schedule-sends! r))
    (when-let [f (:tx-listen cfg)]
      (let [tx-data (merge env
                      {:old-state old-state
                       :new-state @(:state cfg)})]
        (f tx-data {:tx    tx
                    :ret   v
                    :sends snds})))
    v))

(defn transact!
  "Given a reconciler or component run a transaction. tx is a parse expression
   that should include mutations followed by any necessary read. The reads will
   be used to trigger component re-rendering.

   Example:

     (om/transact! widget
       '[(do/this!) (do/that!)
         :read/this :read/that])"
  ([x tx]
   {:pre [(or (om/component? x)
            (om/reconciler? x))
          (vector? tx)]}
   (let [tx (cond-> tx
              (and (om/component? x) (satisfies? om/Ident x))
              (om/annotate-mutations (om/get-ident x)))]
     (cond
       (om/reconciler? x) (transact* x nil nil tx)
       (not (om/iquery? x)) (do
                              (when (om/some-iquery? x) (log/error
                                                          (str "transact! should be called on a component"
                                                            "that implements IQuery or has a parent that"
                                                            "implements IQuery")))
                              (transact* (om/get-reconciler x) nil nil tx))
       :else (do
               (loop [p (#'om/parent x) x x tx tx]
                 (if (nil? p)
                   (let [r (om/get-reconciler x)]
                     (transact* r x nil tx))
                   (let [[x' tx] (if #?(:clj  (satisfies? om/ITxIntercept p)
                                        :cljs (implements? om/ITxIntercept p))
                                   [p (om/tx-intercept p tx)]
                                   [x tx])]
                     (recur (#'om/parent p) x' tx))))))))
  ([r ref tx]
   (transact* r nil ref tx)))

