;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Шаблони
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftemplate number (slot value))
(deftemplate symbol-mapped (slot value) (slot symbol))
(deftemplate transition
  (slot from)
  (slot to)
  (slot pair-id))
(deftemplate status (slot stage))
(deftemplate sorted-list (multislot values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Функції
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffunction read-numbers-from-file (?filename)
  (bind ?opened (open ?filename filein))
  (if (eq ?opened FALSE) then
    (printout t "Не вдалося відкрити файл: " ?filename crlf)
    (return))
  (loop-for-count (?i 1 10000)
    (bind ?val (read filein))
    (if (eq ?val EOF) then (return))
    (assert (number (value ?val))))
  (close filein)
  (printout t "Дані зчитано з файлу: " ?filename crlf)
)

(deffunction insert-in-order (?val ?sorted)
  (bind ?result (create$))
  (bind ?inserted FALSE)
  (foreach ?x ?sorted
    (if (and (not ?inserted) (< ?val ?x)) then
      (bind ?result (create$ ?result ?val))
      (bind ?inserted TRUE))
    (bind ?result (create$ ?result ?x)))
  (if (not ?inserted) then
    (bind ?result (create$ ?result ?val)))
  ?result)

;; Головна логіка перетворення чисел у символи
(deffunction assign-symbols (?vals ?alphabet)
  (bind ?count (length$ ?alphabet))
  (bind ?min (nth$ 1 ?vals))
  (bind ?max (nth$ (length$ ?vals) ?vals))
  (bind ?step (/ (- ?max ?min) ?count))

  ;; Створення інтервалів
  (bind ?intervals (create$))
  (bind ?i 1)
  (while (<= ?i ?count)
    (bind ?start (+ ?min (* (- ?i 1) ?step)))
    (bind ?end (+ ?start ?step))
    (bind ?sym (nth$ ?i ?alphabet))
    (bind ?intervals (create$ ?intervals ?start ?end ?sym))
    (bind ?i (+ ?i 1)))

  ;; Прив'язка чисел
  (do-for-all-facts ((?n number)) TRUE
    (bind ?v ?n:value)
    (bind ?j 0)
    (while (< ?j (* ?count 3))
      (bind ?a (nth$ (+ ?j 1) ?intervals))
      (bind ?b (nth$ (+ ?j 2) ?intervals))
      (bind ?s (nth$ (+ ?j 3) ?intervals))
      (if (or (and (>= ?v ?a) (< ?v ?b))
              (and (= ?v ?max) (= ?b ?max))) then
        (assert (symbol-mapped (value ?v) (symbol ?s)))
        (bind ?j (* ?count 3))) ; вихід з циклу
      (bind ?j (+ ?j 3)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Правила
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule sort-values
  =>
  (bind ?raw (create$))
  (do-for-all-facts ((?n number)) TRUE
    (bind ?raw (create$ ?raw ?n:value)))
  (bind ?sorted (create$))
  (foreach ?val ?raw
    (bind ?sorted (insert-in-order ?val ?sorted)))
  (assert (sorted-list (values ?sorted)))
  (assert (status (stage ready)))
)

(defrule map-values-to-symbols
  ?sorted <- (sorted-list (values $?vals))
  (status (stage ready))
  =>
  (bind ?alphabet (create$ a b c d e f g h i j)) ;; Алфавіт можна змінювати
  (assign-symbols ?vals ?alphabet)
)

(defrule build-transitions
  =>
  (bind ?all (find-all-facts ((?s symbol-mapped)) TRUE))
  (bind ?len (length$ ?all))
  (loop-for-count (?i 1 (- ?len 1))
    (bind ?from (fact-slot-value (nth$ ?i ?all) symbol))
    (bind ?to   (fact-slot-value (nth$ (+ ?i 1) ?all) symbol))
    (assert (transition (from ?from) (to ?to) (pair-id ?i))))
)


(defrule print-symbol-sequence
  =>
  (printout t crlf "Лінгвістичний ряд: ")
  (do-for-all-facts ((?s symbol-mapped)) TRUE
    (printout t ?s:symbol " "))
  (printout t crlf)
)

(defrule print-transition-matrix
  =>
  (bind ?alphabet (create$ a b c d e f g h i j))
  (printout t crlf "Матриця передування:" crlf)
  (printout t "     ")
  (foreach ?col ?alphabet
    (printout t "  " ?col))
  (printout t crlf)

  (foreach ?row ?alphabet
    (printout t ?row ": ")
    (foreach ?col ?alphabet
      (bind ?count (length$ (find-all-facts ((?t transition))
                              (and (eq ?t:from ?row) (eq ?t:to ?col)))))
      (printout t "  " ?count))
    (printout t crlf))
)