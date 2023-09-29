(ns auxon.clj-cucumber
  (:require [clojure.string :as str])
  (:import (io.cucumber.core.backend Backend
                                     HookDefinition
                                     StepDefinition
                                     Snippet)
           (io.cucumber.core.options RuntimeOptions)
           (io.cucumber.core.runtime BackendSupplier
                                     FeatureSupplier)
           (io.cucumber.core.gherkin Feature)
           (io.cucumber.core.snippets SnippetGenerator
                                      SnippetType
                                      Joiner)
           (io.cucumber.cucumberexpressions ParameterTypeRegistry)
    #_(io.cucumber.core.stepexpression ArgumentMatcher
                                       StepExpressionFactory
                                       StepTypeRegistry)
           (java.net URI)
           (java.util.function Supplier)))

(def ^:dynamic *current-scenario-name* nil)

;(def ^:private step-expr-factory
;  (StepExpressionFactory.
;    (TypeRegistry. java.util.Locale/ENGLISH)))

(defn- make-step-def [pattern step-fn arg-count file line state-atom]
  (let [#_#_expression (.createExpression step-expr-factory (str pattern))
        #_#_arg-matcher (ExpressionArgumentMatcher. expression)
        #_#_arg-types (make-array java.lang.reflect.Type 0)]
    (reify StepDefinition

      #_(matchedArguments [_ step]
                          (.argumentsFrom arg-matcher step arg-types))

      (parameterInfos [_]
        nil)

      (getLocation [_]
        (str file ":" line))

      (isDefinedAt [_ stack-trace-element]
        (and (= (.getLineNumber stack-trace-element) line)
             (= (.getFileName stack-trace-element) file)))

      (execute [_ args]
        (let [new-state (apply step-fn @state-atom args)]
          (reset! state-atom new-state))))))

(defn- make-hook-def [order f file line state-atom]
  (reify HookDefinition
    (getLocation [_] (str file ":" line))
    (isDefinedAt [_ stack-trace-element]
      (and (= (.getLineNumber stack-trace-element) line)
           (= (.getFileName stack-trace-element) file)))
    (execute [_ scenario]
      (binding [*current-scenario-name* (.getName scenario)]
        (let [new-state (f @state-atom)]
          (reset! state-atom new-state))))
    (getTagExpression [_] nil)
    (getOrder [_] order)))

(def ^:private snippet-generator
  (SnippetGenerator.
    (reify Snippet
      (template [_]
        ;; {0} : Step Keyword</li>
        ;; {1} : Value of {@link #escapePattern(String)}</li>
        ;; {2} : Function name</li>
        ;; {3} : Value of {@link #arguments(Map)}</li>
        ;; {4} : Regexp hint comment</li>
        ;; {5} : value of {@link #tableHint()} if the step has a table</li>

        (str
          "(step :{0} #\"^{1}$\"\n"
          "      (fn {2} [state {3}]\n"
          "        (comment  {4})\n"
          "        (throw (cucumber.api.PendingException.))))\n"))

      (tableHint [_]
        nil)

      (arguments [_ argumentTypes]
        (str/join " " (keys argumentTypes)))

      (escapePattern [_ pattern]
        (str/replace (str pattern) "\"" "\\\"")))
    (ParameterTypeRegistry. (java.util.Locale/ENGLISH))))


;(def ^:private clj-function-name-generator
;  (reify Joiner
;    (concatenate [_ words]
;      (str/join "-" words))))

(defn- create-clj-backend [steps-or-hooks state-atom]
  (let [{steps :step, hooks :hook} (group-by :type steps-or-hooks)]
    (reify Backend
      (loadGlue [this glue gluePaths]
        ;; register hooks
        (doseq [{:keys [phase order fn file line]} hooks
                :let [hook-def (make-hook-def order fn file line state-atom)]]
          (case phase
            :before (.addBeforeHook glue hook-def)
            :after (.addAfterHook glue hook-def)
            :before-step (.addBeforeStepHook glue hook-def)
            :after-step (.addAfterStepHook glue hook-def)))

        ;; register steps
        (doseq [{:keys [kw pattern fn file line]} steps]
          (.addStepDefinition glue (make-step-def pattern fn
                                                  nil       ; arg-count
                                                  file line state-atom))))

      (buildWorld [this])
      (disposeWorld [this])

      ;; List<String> getSnippet(PickleStep step, String keyword, FunctionNameGenerator functionNameGenerator);
      ;; List<String> getSnippet(Step step, SnippetType snippetType);
      #_(getSnippet [this step _]
                    (.getSnippet step SnippetType/UNDERSCORE)))))

(defn- create-cucumber-runtime [args steps state-atom]
  (let [feature-file (last args)
        backend (create-clj-backend steps state-atom)]
    (println args)
    (.. (io.cucumber.core.runtime.Runtime/builder)
        ;(withRuntimeOptions (RuntimeOptions.))
        (withFeatureSupplier (reify FeatureSupplier
                               (get [_]
                                 [(reify Feature
                                    (getPickleAt [_] nil)
                                    (getPickles [_] [])
                                    (getUri [_] (URI. feature-file))
                                    (getSource [_] feature-file)
                                    (getParsedEvents [_] []))])))
        (withClassLoader (reify Supplier
                           (get [_]
                             (.getContextClassLoader (Thread/currentThread)))))
        (withBackendSupplier (reify BackendSupplier
                               (get [_] [backend])))
        (build))))

;;; Public api

(defmacro step
  "Create a step map, with line and file filled in.

   - `kw`: :Given, :When or :Then
   - `pattern`: The regex to match for this step
   - `fn`: The function to call when executing this step.
           Subgroups matched in `pattern` are provided as parameters."
  [kw pattern fn]
  (let [line (:line (meta &form))]
    `{:type    :step
      :kw      ~kw
      :pattern ~pattern
      :fn      ~fn
      :line    ~line
      :file    ~*file*}))

;; phase: :before :after :before-step :after-step
(defmacro hook
  "Create a hook map"
  [phase fn]
  (let [line (:line (meta &form))]
    `{:type  :hook
      :phase ~phase
      :order 0
      :fn    ~fn
      :line  ~line
      :file  ~*file*}))

(defn run-cucumber
  "Run the cucumber features at `features-path` using the given `steps`.

  `steps` should be a sequence of step definition maps; these these can be
  created easily using the `step` macro."
  [features-path steps]
  (let [args       ["--plugin" "pretty"
                    "--monochrome"
                    features-path]
        state-atom (atom nil)
        runtime    (create-cucumber-runtime args steps state-atom)]
    (.run runtime)
    (.exitStatus runtime)))
