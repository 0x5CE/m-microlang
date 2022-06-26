(ns ambitious.core-test
  (:require [clojure.test :refer :all]
            [ambitious.core :refer :all]))

(deftest def-test
  (defstackfn f [] "ok")
  (let [out (f)]
    (testing "Testing define" 
      (is (= out "ok")))))

(deftest vars-stack-func-test
  (defstackfn f [!var1 !var2]
    !var1
    100
    <pop>
    !var2
    (invoke> * 2)
  )
  (let [out (f 5 6)]
    (testing "Testing variables, stack, and functions" 
      (is (= out 30)))))

(deftest if-test
  (defstackfn f [] 
    100
    (if> 
      15
    else>
      20
    )
  )
  (let [out (f)]
    (testing "Testing if, else" 
      (is (= out 15)))))

(deftest standard-test
  (defstackfn f
    [!a !b !c]
    !a
    !b
    (invoke> + 2)
    !v1+
    !c
    !c
    <pop>
    2
    (invoke> * 2)
    !v2+
    (invoke> = 2)
    (if>
      !v1
      !v2
      (invoke> - 2)
    else>
      "false!!"
      (invoke> println 1)
      <pop>
      !v1
      !v2
      (invoke> * 2)
    )
  )
  (let [out (f 1 2 4)]
    (testing "Testing standard statements" 
      (is (= out 24)))))