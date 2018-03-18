
## atom?

原子数据，非复合数据，非空表，即整数、字符串等基本数据。

    (define (atom? x)
      (and (not (pair? x)) (not (null? x))))

    (atom? 10)         ;; #t
    (atom? "hello")    ;; #t
    (atom? (list 1))   ;; list, #f
    (atom? '(1 2))     ;; list, #f
    (atom? '())        ;; empty list, #f

* `'atom` 是原子吗？

    是的，因为 `'atom` 是字符串。

* `'turkey` 是原子吗？

    是的，因为 `'turkey` 是字符串。


## list?

列表，包含空表、以及原子数据组成的列表。

    (list? '())    ;; #t
    (list? '(10))  ;; #t

* `'(atom)` 是列表吗？

    是的，这个列表包含一个字符串 `"atom"`。

* `'((atom turkey) or)` 是列表吗？

   是的，这个列表包含一个列表和一个字符串。


## car 仅针对非空列表

* `(car '(a b c))` 的结果是什么？

    `a`

* `(car 'hotdog)` 结果是什么？

    错误，`'hotdog` 不是列表。

* `(car '())` 结果是什么？

    错误，不是非空列表。

* `(car (car l))` 结果是什么？ 其中 l 是 `'(((hotdog)) (and) pickle)`。

    `(hotdog)`，因为 `(car l)` 是 `((hotdog))`，包含一个字符串的列表的列表。


## cdr 仅用于非空列表，其结果为列表

    (cdr '(a b c))          ;; (b c)
    (cdr '((a b c) x y z))  ;; (x y z)
    (cdr '(hamburger))      ;; ()

* `(cdr 'hotdog)` 的结果是什么？

    错误，不是列表。

* `(cdr '())` 的结果是什么？

    错误，不是非空列表。

