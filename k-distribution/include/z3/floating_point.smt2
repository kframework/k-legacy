(set-option :auto-config false)
(set-option :smt.mbqi false)

; help z3
(assert (forall ((e1 (_ FP 11 53)) (e2 (_ FP 11 53))) (= (and (<= e1 e2) (not (== e1 e2))) (> e1 e2))))
(assert (forall ((e1 (_ FP 11 53)) (e2 (_ FP 11 53))) (= (and (distinct (< e1 e2) true) (not (== e1 e2))) (> e1 e2))))

; set axioms
(declare-sort FloatSet)

(declare-fun smt_set_cup (FloatSet FloatSet) FloatSet)
(declare-fun smt_set_ele ((_ FP 11 53)) FloatSet)
(declare-fun smt_set_emp () FloatSet)
(declare-fun smt_set_dif (FloatSet FloatSet) FloatSet)
(declare-fun smt_set_mem ((_ FP 11 53) FloatSet) Bool)
(declare-fun smt_set_lt  (FloatSet FloatSet) Bool)
(declare-fun smt_set_le  (FloatSet FloatSet) Bool)

(assert (forall ((s1 FloatSet) (s2 FloatSet) (s3 FloatSet)) (= (smt_set_cup (smt_set_cup s1 s2) s3) (smt_set_cup s1 (smt_set_cup s2 s3)))))
(assert (forall ((s1 FloatSet) (s2 FloatSet)) (= (smt_set_cup s1 s2) (smt_set_cup s2 s1))))
(assert (forall ((s FloatSet)) (= (smt_set_cup s s) s)))

(assert (forall ((e1 (_ FP 11 53)) (e2 (_ FP 11 53))) (= (= (smt_set_ele e1) (smt_set_ele e2)) (== e1 e2))))
(assert (forall ((e (_ FP 11 53))) (not (= (smt_set_ele e) smt_set_emp))))

(assert (forall ((s1 FloatSet) (s2 FloatSet) (s3 FloatSet)) (= (smt_set_lt (smt_set_cup s1 s2) s3) (and (smt_set_lt s1 s3) (smt_set_lt s2 s3)))))
(assert (forall ((s1 FloatSet) (s2 FloatSet) (s3 FloatSet)) (= (smt_set_lt s1 (smt_set_cup s2 s3)) (and (smt_set_lt s1 s2) (smt_set_lt s1 s3)))))
(assert (forall ((e1 (_ FP 11 53)) (e2 (_ FP 11 53))) (= (smt_set_lt (smt_set_ele e1) (smt_set_ele e2)) (< e1 e2))))
(assert (forall ((s FloatSet)) (smt_set_lt s smt_set_emp)))
(assert (forall ((s FloatSet)) (smt_set_lt smt_set_emp s)))

(assert (forall ((e (_ FP 11 53)) (s1 FloatSet) (s2 FloatSet)) (! (implies (and (smt_set_lt s1 (smt_set_ele e)) (smt_set_lt (smt_set_ele e) s2)) (smt_set_lt s1 s2))
    :pattern ((smt_set_lt s1 (smt_set_ele e)) (smt_set_lt (smt_set_ele e) s2))
    :pattern ((smt_set_ele e) (smt_set_lt s1 s2))
)))
;(assert (forall ((e (_ FP 11 53)) (s1 FloatSet) (s2 FloatSet)) (! (implies (and (smt_set_lt s1 (smt_set_ele e)) (smt_set_le (smt_set_ele e) s2)) (smt_set_lt s1 s2))
;    :pattern ((smt_set_lt s1 (smt_set_ele e)) (smt_set_le (smt_set_ele e) s2))
;    :pattern ((smt_set_ele e) (smt_set_lt s1 s2))
;)))
;(assert (forall ((e (_ FP 11 53)) (s1 FloatSet) (s2 FloatSet)) (! (implies (and (smt_set_le s1 (smt_set_ele e)) (smt_set_lt (smt_set_ele e) s2)) (smt_set_lt s1 s2))
;    :pattern ((smt_set_le s1 (smt_set_ele e)) (smt_set_lt (smt_set_ele e) s2))
;    :pattern ((smt_set_ele e) (smt_set_lt s1 s2))
;)))

(assert (forall ((s1 FloatSet) (s2 FloatSet) (s3 FloatSet)) (= (smt_set_le (smt_set_cup s1 s2) s3) (and (smt_set_le s1 s3) (smt_set_le s2 s3)))))
(assert (forall ((s1 FloatSet) (s2 FloatSet) (s3 FloatSet)) (= (smt_set_le s1 (smt_set_cup s2 s3)) (and (smt_set_le s1 s2) (smt_set_le s1 s3)))))
(assert (forall ((e1 (_ FP 11 53)) (e2 (_ FP 11 53))) (= (smt_set_le (smt_set_ele e1) (smt_set_ele e2)) (<= e1 e2))))
(assert (forall ((s FloatSet)) (smt_set_le s smt_set_emp)))
(assert (forall ((s FloatSet)) (smt_set_le smt_set_emp s)))

(assert (forall ((e (_ FP 11 53)) (s1 FloatSet) (s2 FloatSet)) (! (implies (and (smt_set_le s1 (smt_set_ele e)) (smt_set_le (smt_set_ele e) s2)) (smt_set_le s1 s2))
    :pattern ((smt_set_lt s1 (smt_set_ele e)) (smt_set_lt (smt_set_ele e) s2))
    :pattern ((smt_set_ele e) (smt_set_lt s1 s2))
)))

;(assert (forall ((s1 FloatSet) (s2 FloatSet)) (implies (smt_set_lt s1 s2) (smt_set_le s1 s2))))

(assert (forall ((e (_ FP 11 53)) (s FloatSet)) (implies (smt_set_lt (smt_set_ele e) s) (not (smt_set_mem e s)))))
(assert (forall ((e (_ FP 11 53)) (s FloatSet)) (implies (smt_set_lt s (smt_set_ele e)) (not (smt_set_mem e s)))))

(assert (forall ((e (_ FP 11 53)) (s FloatSet)) (implies (smt_set_lt (smt_set_ele e) s) (= (smt_set_dif s (smt_set_ele e)) s))))
(assert (forall ((e (_ FP 11 53)) (s FloatSet)) (implies (smt_set_lt s (smt_set_ele e)) (= (smt_set_dif s (smt_set_ele e)) s))))

; sequence axioms
(declare-sort IntSeq)

(declare-fun smt_seq_concat (IntSeq IntSeq) IntSeq)
(declare-fun smt_seq_elem (Int) IntSeq)
(declare-fun smt_seq_nil () IntSeq)
(declare-fun smt_seq_len (IntSeq) Int)

