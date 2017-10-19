(ns fulcro.client.om-upgrade
  (:require [om.next :as om]
            [om.next.protocols :as p]
            [om.util :as util]
    #?(:cljs goog.log)
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
   (let [class                 (-> factory meta :class)
         qualifier             (-> factory meta :qualifier)
         static-query #?(:clj  ((-> class meta :query) class)
                         :cljs (om/query class))
         static-params #?(:clj ((-> class meta :params) class)
                          :cljs (om/params class))
         queryid               (query-id class qualifier)
         query                 (or (denormalize-query state-map queryid) static-query)
         params                static-params
         c                     (-> static-query meta :component)]
     (assert (nil? c) (str "Query violation, " class, " reuses " c " query"))
     (with-meta (#'om/bind-query query params) {:component class
                                                :queryid   (query-id class qualifier)}))))


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
    (map? query) (-> query keys set) ; a union component, which has a map for a query
    :else #{}))

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
                          (om/invariant (util/ident? ident)
                            (str "malformed Ident. An ident must be a vector of "
                              "two elements (a keyword and an EDN value). Check "
                              "the Ident implementation of component `"
                              (.. c -constructor -displayName) "`."))
                          (om/invariant (some? (second ident))
                            (str "component " (.. c -constructor -displayName)
                              "'s ident (" ident ") has a `nil` second element."
                              " This warning can be safely ignored if that is intended."))
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

