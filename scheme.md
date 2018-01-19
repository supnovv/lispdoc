
Introduction

Scheme is a statically scoped and properly tail-recursive dialect of the Lisp programming language invented by Guy Lewis Steels Jr. and Gerald Jay Sussman. It was designed to have an exceptionally clear and simple semantics and few different ways to form expressions. A wide variety of programming paradigms, including functional, imperative, and message passing styles, find convenient expression in Scheme.

Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary. Scheme demonstrates that a very small number of rules for forming expressions, with no restrictions on how they are composed, suffice to form a practical and efficient programming language that is flexible enough to support most of the major programming paradigms in use toady.

Following Algol, Scheme is a statically scoped programming language. Each use of a variable is associated with a lexically apparent building of that variable. Scheme has latent as opposed to manifest types. Types are associated with objects (also called values) rather than with variables. (Some authors refer to languages with latent types as untyped, weakly typed or dynamically typed languages.) Other languages with latent types are Python, Ruby, Smalltalk, and other dialets of Lisp. Languages with manifest types (sometimes referred to as strongly typed or statically typed languages) include Algol 60, C, C#, Java, Haskell, and ML.

All objects created in the course of a Scheme computation, including procedures and continuations, have unlimited extent. No Scheme object is ever destroyed. The reason that implementations of Scheme do not run out of storage is that they are permitted to reclaim the storage occupied by an object if they can prove that the object cannot possibly matter to any future computation. Other languages in which most objects have unlimited extent include C#, Java, Haskell, most Lisp dialects, ML, Python, Ruby, and Samlltalk.

Implementations of Scheme must be properly tail-recursive. This allows the execution of an iterative computation in constant space, even if the iterative computation is described by a syntactically recursive procedure. Thus with a properly tail-recursive implementation, iteration can be expressed using the ordinary procedure-call mechanics, so that special iteration constructs are useful only as syntactic sugar.

Scheme was one of the first languages to support procedures as objects in their own right. Procedures can be created dynamically, stored in data structures, returned as results of procedures, and so on. Other languages with these properties include Common Lisp, Haskell, ML, Ruby, and Samlltalk. One distinguishing feature of Scheme is that continuations, which in most other languages only operate behind the scenes, also have "first-class" status. Furst-class continuations are useful for implementing a wide variety of advanced control constructs, including non-local exits, back-tracking, and coroutines.

In Scheme, the argument expressions of a procedure call are evaluated before the procedure gains control, whether the procedure needs the result of the evaluation or not. C, C#, Common Lisp, Python, Ruby, and Smalltalk are other languages that always evaluate argument expressions before invoking a procedure. This is distinct from by lazy-evaluation semantics of Haskell, or the call-by-name semantics of Algol 60, where an argument expression is not evaluated unless its value is needed by the procedure.

Scheme's model of arithmetic provides a rich set of numerical types and operations on them. Furthermore, it distinguishes exact and inexact number object. Essentially, an exact number object corresponds to a number exactly, and an inexact number object is the result of a computation that involved rounding or other errors.


Basic types

Scheme programs manipulate objects, which are also referred to as values. Scheme objects are organized into sets of values called types. Note: As Scheme is latently typed, the use of the term type in this report differs the use of the term in the context of other languages, particularly those with manifest typing.

**Booleans** A boolean is a truth value, and can be either true or false. In Scheme, the object for "false" is written #f. The object for "true" is written #t. In most places where a truth value is expected, however, any object different from #f counts as true.





**Numbers** Scheme supports a rich variety of numerical data types, including objects representing integers of arbitrary precision, rational numbers, complex numbers, and inexact numbers of various kinds.





**Characters** Scheme characters mostly correspond to textual characters. More precisely, they are isomorphic to the scalar values of the Unicode standard. 




**Strings** Strings are finite sequences of characters with fixed length and thus represent arbitrary Unicode texts.






**Symbols** A symbol is an object representing a string, the symbol's name. Unlike strings, two symbols whose names are spelled the same way are never distinguishable. Symbols are useful for many applications; for instance, they may be used the way enumerated values are used in other languages.



Pairs and lists, Vectors, Procedures




Expressions

The most important elements of Scheme code are expressions. Expressions can be evaluated producing a value. The most fundamental expressions are literal expressions (like #t, 23). Compound expressions are formed by placing parentheses around their subexpressions. The first subexpression identifies an operation; the remaing subexpression are operands to the operation, like (+ 23 42), (+ 14 15 (* 23 42)). As these examples indicate, compound expressions in Scheme are always written using the same prefix notation. As a consequence, the parentheses are needed to indicate structure. Consequently, "superfluous" parentheses, which are often permissible in mathematical notation and also in many programming languages, are not allowed in Scheme. As in many other languages, whitespace (inluding line endings) is not significant when it separates subexpressions of an expression, and can be used to indicate structure.


Variables

Scheme allows identifies to stand for locations containg values. These identifiers are called variables. In many cases, specially when the location's value is never modified after its creation, it is useful to think of the variable as standing for the value directly.

(let ((x 23)
      (y 42))
  (+ x y))           => 65

The variables bound by a let expression like above are local, because their bindings are visible only in the let's body (+ x y), and only there. Scheme also allows creating top-level bindings for identifiers as follows. Actually, these are "top-level" in the body of a top-level program or library.

(define x 23)
(define y 42)
(+ x y)             => 65

The first two parenthesized structures are definitions, they create top-level bindings. Definitions are not expressions, and cannot appear in all places where an expression can occur. Moreover, a definition has no value. Bindings follow the lexical structure of the program. When several bindings with the same name exist, a variable refers to the binding that is closest to it, starting with its occurrence in the program and going from inside to outside, and referring to a top-level binding if no local binding can be found along the way.

(define x 23)
(define y 42)
(let ((y 43))
  (+ x y))         => 66

(let ((y 43))
  (let ((y 44))
    (+ x y)))     => 67




