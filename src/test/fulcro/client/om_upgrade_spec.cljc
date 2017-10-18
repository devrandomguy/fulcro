(ns fulcro.client.om-upgrade-spec
  (:require [fulcro-spec.core :refer [specification behavior assertions provided component when-mocking]]
            [om.next :as om :refer [defui]]
            [fulcro.client.om-upgrade :as om+]
    #?@(:cljs [[goog.object :as gobj]])
            ))

(defui A)

(specification "Query IDs"
  (assertions
    "Start with the fully-qualifier class name"
    (om+/query-id A nil) => "fulcro$client$om_upgrade_spec$A"
    "Include the optional qualifier"
    (om+/query-id A :x) => "fulcro$client$om_upgrade_spec$A$:x"))

(specification "UI Factory"
  (assertions
    "Adds  react-class to the metadata of the generated factory"
    (some-> (om+/factory A) meta :class) => A
    "Adds an optional qualifier to the metadata of the generated factory"
    (some-> (om+/factory A) meta :qualifier) => nil
    (some-> (om+/factory A {:qualifier :x}) meta :qualifier) => :x)
  (behavior "Adds an internal query id to the props passed by the factory"
    #?(:cljs
       (when-mocking
         (om+/query-id c q) => :ID
         (om+/create-element class props children) => (do
                                                        (assertions
                                                          (gobj/get props "omcljs$queryid") => :ID))

         ((om+/factory A) {}))
       :clj
       (let [class (fn [_ _ props _] (assertions (:omcljs$queryid props) => :ID))]
         (when-mocking
           (om+/query-id c q) => :ID
           (om+/init-local-state c) => nil

           ((om+/factory class) {}))))))

(defui Q
  static om/IQuery
  (query [this] [:a :b]))

(def ui-q (om+/factory Q))

(defui UnionChildA
  static om/IQuery
  (query [this] [:L]))

(def ui-a (om+/factory UnionChildA))

(defui UnionChildB
  static om/IQuery
  (query [this] [:M]))

(def ui-b (om+/factory UnionChildB))

(defui Union
  static om/IQuery
  (query [this] {:u1 (om+/get-query* this ui-a)
                 :u2 (om+/get-query* this ui-b)}))

(def ui-union (om+/factory Union))

(defui Child
  static om/IQuery
  (query [this] [:x]))

(def ui-child (om+/factory Child))

(defui Root
  static om/IQuery
  (query [this] [:a
                 {:join (om+/get-query* this ui-child)}
                 {:union (om+/get-query* this ui-union)}]))

(def ui-root (om+/factory Root))

(specification "link-query" :focused
  (assertions
    "Replaces nested queries with their string ID"
    (om+/link-query (om+/get-query* {} ui-root)) => [:a {:join (om+/query-id Child nil)} {:union (om+/query-id Union nil)}]))

(specification "normalize-query" :focused
  (let [union-child-a-id (om+/query-id UnionChildA nil)
        union-child-b-id (om+/query-id UnionChildB nil)
        child-id         (om+/query-id Child nil)
        root-id          (om+/query-id Root nil)
        union-id         (om+/query-id Union nil)
        existing-query   {:id    union-child-a-id
                          :query [:OTHER]}]
    (assertions
      "Adds simple single-level queries into app state under a reserved key"
      (om+/normalize-query {} (om+/get-query* {} ui-a)) => {::om+/queries {union-child-a-id
                                                                           {:id    union-child-a-id
                                                                            :query [:L]}}}
      "Single-level queries are not added if a query is already set in state"
      (om+/normalize-query {::om+/queries {union-child-a-id existing-query}} (om+/get-query* {} ui-a)) => {::om+/queries {union-child-a-id existing-query}}
      "More complicated queries normalize correctly"
      (om+/normalize-query {} (om+/get-query* {} ui-root))
      => {::om+/queries {root-id          {:id    root-id
                                           :query [:a {:join child-id} {:union union-id}]}
                         union-id         {:id    union-id
                                           :query {:u1 union-child-a-id :u2 union-child-b-id}}
                         union-child-b-id {:id    union-child-b-id
                                           :query [:M]}
                         union-child-a-id {:id    union-child-a-id
                                           :query [:L]}
                         child-id         {:query [:x]
                                           :id    child-id}}})))

(specification "get-query*" :focused
  (assertions
    "Obtains the static query from a given class"
    (om+/get-query* Q) => [:a :b]
    "Obtains the static query when the state has no stored queries"
    (om+/get-query* {} ui-q) => [:a :b])
  (let [query        (om+/get-query* {} ui-root)
        top-level    query
        join-target  (get-in query [1 :join])
        union-target (get-in query [2 :union])
        union-left   (get union-target :u1)
        union-right  (get union-target :u2)]
    (assertions
      "Places a query ID on the metadata of each component's portion of the query"
      (-> top-level meta :queryid) => "fulcro$client$om_upgrade_spec$Root"
      (-> join-target meta :queryid) => "fulcro$client$om_upgrade_spec$Child"
      (-> union-target meta :queryid) => "fulcro$client$om_upgrade_spec$Union"
      (-> union-left meta :queryid) => "fulcro$client$om_upgrade_spec$UnionChildA"
      (-> union-right meta :queryid) => "fulcro$client$om_upgrade_spec$UnionChildB"))
  (behavior "Pulls a denormalized query from app state if one exists."
    (let [app-state (om+/normalize-query {} (om+/get-query* {} ui-root))
          app-state (assoc-in app-state [::om+/queries (om+/query-id UnionChildA nil) :query] [:UPDATED])]
      (assertions
        (om+/get-query* app-state ui-root) => [:a {:join [:x]} {:union {:u1 [:UPDATED] :u2 [:M]}}]))
    ))

(comment
  (-> (om+/get-query* {} ui-root) (om/query->ast) (clojure.pprint/pprint))
  (clojure.pprint/pprint (om/query->ast [:a {:j [:x]} {:u {:u1 [:l] :u2 [:m]}}])))




