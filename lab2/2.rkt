#lang racket

;; Зчитування чисел із текстового файлу
(define (read-numbers-from-file path)
  ;; Зчитуємо вміст файлу як рядок
  (define content (file->string path))
  ;; Розбиваємо рядок по пробілах
  (define parts (string-split content))
  ;; Перетворюємо кожну частину в число
  (map string->number parts))

;; Сортування числового ряду
(define (sort-numeric-sequence seq)
  (sort seq <))

;; Побудова інтервалів за рівномірним розподілом
(define (build-intervals sorted-seq alphabet)
  (define min-val (first sorted-seq))
  (define max-val (last sorted-seq))
  (define range (- max-val min-val))
  (define step (/ range (length alphabet)))
  ;; Повертаємо список інтервалів у вигляді пар [a, b)
  (for/list ([i (in-range (length alphabet))])
    (list (+ min-val (* i step)) (+ min-val (* (+ i 1) step)))))

;; Знаходження індексу інтервалу, в який потрапляє значення
(define (find-interval-index value intervals)
  (let loop ([i 0] [ints intervals])
    (cond
      [(null? ints) (- (length intervals) 1)] ; якщо не знайдено, віддати останній індекс
      [(and (<= (first (first ints)) value)
            (< value (second (first ints)))) i]
      ;; Додаткова перевірка на крайній правий кінець
      [(and (= i (- (length intervals) 1))
            (= value (second (first ints)))) i]
      [else (loop (+ i 1) (rest ints))])))

;; Відображення чисел на символи алфавіту
(define (map-numbers-to-symbols seq intervals alphabet)
  (map (lambda (x)
         (list-ref alphabet (find-interval-index x intervals)))
       seq))

;; Побудова матриці передування
(define (build-transition-matrix symbol-seq alphabet)
  (define size (length alphabet))
  ;; Створюємо матрицю як вектор векторів, заповнений нулями
  (define table
    (build-vector size
      (lambda (_)
        (make-vector size 0))))
  ;; Проходимо по всіх парах символів у ряді
  (for ([i (in-range (- (length symbol-seq) 1))])
    (let* ([a (index-of alphabet (list-ref symbol-seq i))]
           [b (index-of alphabet (list-ref symbol-seq (+ i 1)))])
      (vector-set! (vector-ref table a) b
                   (+ 1 (vector-ref (vector-ref table a) b)))))
  table)

;; Основна функція
(define (run-lab2 filepath alphabet)
  (define numeric-seq (read-numbers-from-file filepath))
  (define sorted (sort-numeric-sequence numeric-seq))
  (define intervals (build-intervals sorted alphabet))
  (define symbols (map-numbers-to-symbols numeric-seq intervals alphabet))
  (define matrix (build-transition-matrix symbols alphabet))
  (values symbols matrix))

;; Функція для виводу результатів
(define (print-results symbols matrix alphabet)
  (displayln "Лінгвістичний ряд:")
  (displayln symbols)
  (newline)
  (displayln "Матриця передування:")
  ;; Виводимо заголовки стовпців
  (display "   ")
  (for-each (lambda (c) (display (format "~a  " c))) alphabet)
  (newline)
  ;; Виводимо кожен рядок з матриці
  (for ([i (in-range (length alphabet))])
    (display (format "~a: " (list-ref alphabet i)))
    (for ([j (in-range (length alphabet))])
      (define val (vector-ref (vector-ref matrix i) j))
      (display (format "~a  " val)))
    (newline)))

;; === Параметри запуску ===
(define alphabet '(A B C)) ;; Можна змінювати розмір і склад
(define filepath "1.txt")  ;; Ім’я вхідного файлу

;; === Запуск ===
(define-values (symbols matrix) (run-lab2 filepath alphabet))
(print-results symbols matrix alphabet)
