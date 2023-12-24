; заготовка "Доктора". Сентябрь 2023
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v3 name #())
)

; вторая функция, запускающая "Доктора", но в многопользовательском режиме
(define (visit-doctor-v2 stop max)
  (let visit ((name (ask-patient-name)) (count 1))
    (if (equal? name stop)
        (println '(time to go home))
        (begin
          (visit-doctor name)
          (if (= count max)
              (println '(time to go home))
              (visit (ask-patient-name) (+ count 1))
          )
        )
    ) 
  )
)

; узнаем имя пациента
(define (ask-patient-name)
  (begin
    (println '(next!))
    (println '(who are you?))
    (print '**)
    (car (read))
  ) 
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline)
            )
            (else (print (reply-v2 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (if (< (vector-length history) 5)
                      (doctor-driver-loop-v2 name (vector-append history (vector user-response)))
                      (doctor-driver-loop-v2 name (vector-append (vector-drop history 1) (vector user-response)))
                  )
             )
       )
      )
)

; функция, использующаяся для фильтрации реплик пользователя от "плохих" слов
; используется в реализации дополнительной стратегии ответов
(define (doctor-driver-loop-v3 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week))
             (newline)
            )
            (else (print (reply-v3 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (if (has-bad-word user-response) ; при наличии стоп-слова в реплике, она не добавляется в историю фраз, а история меняется
                      (if (> (vector-length history) 2)
                          (doctor-driver-loop-v3 name (vector-take history 3))
                          (doctor-driver-loop-v3 name #())
                      )
                      (if (< (vector-length history) 5)
                          (doctor-driver-loop-v3 name (vector-append history (vector user-response)))
                          (doctor-driver-loop-v3 name (vector-append (vector-drop history 1) (vector user-response)))
                      )
                  )
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history)
  (case (random (if (> (vector-length history) 0) 0 1) (if (has-keyword user-response) 4 3))
    ; с равной вероятностью выбирается один из 2-4 способов построения ответа
    ((0) (history-answer user-response history))  ; 3й способ
    ((1) (hedge-answer)) ; 1й способ
    ((2) (qualifier-answer user-response)) ; 2й способ
    ((3) (keyword-answer user-response)) ; 4й способ
  ) 
)

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random-vector '#((please go on)
                              (many people have the same sorts of feelings)
                              (many of my patients have told me the same thing)
                              (please continue)
                              (what do you think can be done about this?)
                              (tell me everything that worries you)
                              (i understand. i will be glad to help you))
         )
)

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
)

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
        (append (pick-random-vector '#((you seem to think that)
                                       (you feel that)
                                       (why do you believe that)
                                       (why do you say that)
                                       (what do you think about the fact that)
                                       (it is good that you decided to tell me that)
                                       (do not worry so much about))
                )
                (change-person user-response)
        )
 )

; замена лица во фразе
(define (change-person phrase)
        (many-replace-v3
		'((am are)
                  (are am)
                  (i you)
                  (me you)
                  (mine yours)
                  (my your)
                  (myself yourself)
                  (you i)
                  (your my)
                  (yours mine)
                  (yourself myself)
                  (we you)
                  (us you)
                  (our your)
                  (ours yours)
                  (ourselves yourselves)
                  (yourselves ourselves)
                  (shall will)
                 )
                phrase
        )
 )

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)

(define (many-replace-v2 replaicement-pairs lst)
  (let loop ((phrase lst) (result '()))
    (if (null? phrase)
        (reverse result)
        (let ((pat-rep (assoc (car phrase) replaicement-pairs)))
          (loop (cdr phrase) (if pat-rep
                                  (cons (car (cdr pat-rep)) result)
                                  (cons (car phrase) result) 
                             )
          )    
        )
    )
  )  
)

(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (x)
         (let ((pat-rep (assoc x replacement-pairs)))
           (if pat-rep
               (car (cdr pat-rep))
               x
           )
         )
       )
       lst
  )
)

; 3й способ генерации ответной реплики -- запоминание последних 5 реплик пациента и в ходе беседы возвращение к сказанному пациентом ранее
(define (history-answer user-response history)
  (append '(earlier you said that) (change-person (pick-random-vector history)))
)

; 4й способ генерации ответной реплики - поиск ключевых слов и составление фразы с использованием ключевого слова
; список ключевых слов и ответов на них
(define keywords-structure '#(
                              #( ; начало данных 1й группы
                                #(depressed suicide exams university) ; список ключевых слов 1й группы
                                #( ; список шаблонов для составления ответных реплик 1й группы 
                                  (when you feel depressed, go out for ice cream)
                                  (depression is a disease that can be treated)
                                  (i can reccomend you to go for a walk when you feel bad)
                                  (you should think less about *)
                                 )
                               ) ; завершение данных 1й группы
                              #( ; начало данных 2й группы ...
                                #(mother father parents brother sister uncle aunt grandma grandpa)
                                #(
                                  (tell me more about your * , i want to know all about your *)
                                  (why do you feel that way about your * ?)
                                  (i beleive your * love you)
                                  (what do you think about your *)
                                 )
                               )
                              #(
                                #(university scheme lections)
                                #(
                                  (your education is important)
                                  (how much time do you spend on your studies ?)
                                  (when you feel bad from * , try to change your activity for an hour)
                                  (you will be a demanded spelialist when you graduate from university)
                                 )
                               )
                              #(
                                #(ML DL)
                                #(
                                  (i do not like * too)
                                  (i am agree that * is sad to study)
                                  (sometimes * cat solve really interesting problems)
                                 )
                               )
                              #(
                                #(potato tomato cabbage celery vegetables)
                                #(
                                  (a lot of people does not like *)
                                  (* can be good for your health)
                                  (doctors advise eating vegetables)
                                 )
                               )
                             )
)

; проверка на наличие ключевого слова во фразе пользователя
(define (has-keyword user-response)
  (call/cc
   (lambda (cc-exit)    
     (let loop ((user-response user-response))
       (if (null? user-response)
           #f ; не было найдено ключевое слово
           (begin
             (vector-foldl ; поиск первого слова в ответе среди ключевых слов
              (lambda (i result elem)
                (if (equal? (car user-response) elem)
                    (cc-exit #t) ; завершение при нахождении ключевого слова
                    (void)
                )
              )
              #()
              keywords-list
             )
             (loop (cdr user-response)) ; если поиск оказался неудачным, то переходим ко второму слову и т.д.
           )
       )
     )     
   )
  )
)

; отбор всех ключевых слов во фразе
(define (get-keywords-old user-response)
  (let loop ((user-response user-response) (keywords #()))
    (if (null? user-response)
        keywords
        (loop
         (cdr user-response) 
         (vector-append keywords
                        (vector-foldl
                         (lambda (i result elem)
                           (if (equal? (car user-response) elem) 
                               (vector-append result (vector elem))
                               result
                           )
                         )
                         #()
                         keywords-list
                        )
         )
        )
    )
  )
)

; проверка слова на принадлежность к ключевым
(define (iskeyword? word)
  (if (vector-member word keywords-list)
      #t
      #f
  )
)

; отбор всех ключевых слов во фразе с использованием vector-filter
(define (get-keywords user-response)
  (filter iskeyword? user-response)
)

; так как слово может быть в нескольких шаблонах, нужно объединить некоторые шаблоны при выборе слова
(define (merge-templates keyword)
  (vector-foldl
   (lambda (i result elem)
     (if (vector-member keyword (vector-ref elem 0)) 
         (vector-append result (vector-ref elem 1))
         result
     )
   )
   #()
   keywords-structure
  )
)

; выбор одного слова из списка ключевых слов
(define (get-one-keyword user-response)
  (let ((keywords (get-keywords user-response)))
    (list-ref keywords (random 0 (length keywords)))
  )
)

; основная функция 4го способа генерации реплик
(define (keyword-answer user-response)
  (let ((keyword (get-one-keyword user-response)))
    (many-replace-v3
     (list (list '* keyword)) ; заменяем * на keyword
     (pick-random-vector (merge-templates keyword)) ; в выбранном шаблоне ответа
    )
  )
)

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i 0) (result init))
   (if (= i length) result
    (loop (add1 i) (f i result (vector-ref vctr i)))))))
	
; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
 (let ((length (vector-length vctr)))
  (let loop ((i (sub1 length)) (result init))
   (if (= i -1) result
    (loop (sub1 i) (f i result (vector-ref vctr i)))))))

; сбор всех ключевых слов в один список
(define keywords-list
  (vector-foldl
   (lambda (i result elem)
     (if (vector-member elem result)
         result
         (vector-append result (vector elem))
     )
   )
   #()
   (vector-foldl
    (lambda (j res el)
      (vector-append res (vector-ref el 0))
    )
    #()
    keywords-structure
   )
  )
)

; структура, хранящая информацию о стратегиях ответа
(define strategy-struct
  (vector 
   (vector 
    (lambda (user-response history) (if (> (vector-length history) 0) #t #f))
    10
    (lambda (user-response history) (history-answer user-response history))
   )
   (vector
    (lambda (user-response history) #t)
    1
    (lambda (user-response history) (hedge-answer))
   )
   (vector
    (lambda (user-response history) #t)
    5
    (lambda (user-response history) (qualifier-answer user-response))
   )
   (vector
    (lambda (user-response history) (if (has-keyword user-response) #t #f))
    20
    (lambda (user-response history) (keyword-answer user-response))
   )
  )
)

; фильтрация применимых стратегий ответа
(define (get-applicable-strategies user-response history)
  (vector-filter
   (lambda (elem) ((vector-ref elem 0) user-response history))
   strategy-struct
  )
)

; подсчет всех весов в применимых стратегиях
(define (count-applicable-weights filtered-strategy-struct)
  (vector-foldl
   (lambda (i result elem)
     (+ result (vector-ref elem 1))
   )
   0
   filtered-strategy-struct
  )
)

; выбор ответа с учетом применимости и весов
(define (pick-strategy user-response history)
  (let ((filtered-strategy-struct (get-applicable-strategies user-response history)))
    (let ((num (random 0 (count-applicable-weights filtered-strategy-struct))))
      ;(println num)
      (call/cc
       (lambda (cc-exit)
         (vector-foldl
          (lambda (i result elem)
            (if (< num (+ (vector-ref elem 1) result))
                (cc-exit (vector-ref elem 2))
                (+ result (vector-ref elem 1))
            )
          )
          0
          filtered-strategy-struct
         )
       )
      )
    )
  )
)

; описание функции reply-v2
(define (reply-v2 user-response history)
  ((pick-strategy user-response history) user-response history)
)



;;
;; новая стратегия ответов на реплики пользователя - при обнаружении "плохих" слов менять тему диалога
;;

; список стоп-слов
(define bad-words-list #(selfharm veins throat hang kill alcohol drugs))

; проверка на наличие стоп-слова во фразе пользователя
(define (has-bad-word user-response)
  (call/cc
   (lambda (cc-exit)    
     (let loop ((user-response user-response))
       (if (null? user-response)
           #f
           (begin
             (vector-foldl
              (lambda (i result elem)
                (if (equal? (car user-response) elem)
                    (cc-exit #t)
                    (void)
                )
              )
              #()
              bad-words-list
             )
             (loop (cdr user-response))
           )
       )
     )     
   )
  )
)

; новая стратегия ответа - при встрече стоп-слова предлагается сменить тему диалога
(define (reply-v3 user-response history)
  (if (has-bad-word user-response)
      (begin
        (println '(i think we should not talk about this))
        (println '(let's go a little back))
        ; вместо стандартного ответа берем 3 фразу в истории реплик пользователя, вместо history нужно взять первые две фразы если они есть
        ; если длины истории ответов не хватает, то:
        ; 1) если есть хотя бы одна фраза - использовать ее и обнулить историю
        ; 2) если история пустая, то начать диалог с начала фразой "tell me please about your problem using another words"
        (if (> (vector-length history) 2)
            ((pick-strategy (vector-ref history 2) (vector-take history 2)) (vector-ref history 2) (vector-take history 2))
            (if (> (vector-length history) 0)
                ((pick-strategy (vector-ref history 0) #()) (vector-ref history 0) #())
                '(tell me please about your problem using another words)
            )
        )
      )
      ((pick-strategy user-response history) user-response history)
  )
)


(visit-doctor-v2 'stop 3)