
Introduction

Scheme is a statically scoped and properly tail-recursive dialect of the Lisp programming language invented by Guy Lewis Steels Jr. and Gerald Jay Sussman. It was designed to have an exceptionally clear and simple semantics and few different ways to form expressions. A wide variety of programming paradigms, including functional, imperative, and message passing styles, find convenient expression in Scheme.

Programming languages should be designed not by piling feature on top of feature, but by removing the weaknesses and restrictions that make additional features appear necessary. Scheme demonstrates that a very small number of rules for forming expressions, with no restrictions on how they are composed, suffice to form a practical and efficient programming language that is flexible enough to support most of the major programming paradigms in use toady.

Following Algol, Scheme is a statically scoped programming language. Each use of a variable is associated with a lexically apparent building of that variable. Scheme has latent as opposed to manifest types. Types are associated with objects (also called values) rather than with variables. (Some authors refer to languages with latent types as untyped, weakly typed or dynamically typed languages.) Other languages with latent types are Python, Ruby, Smalltalk, and other dialets of Lisp. Languages with manifest types (sometimes referred to as strongly typed or statically typed languages) include Algol 60, C, C#, Java, Haskell, and ML.

All objects created in the course of a Scheme computation, including procedures and continuations, have unlimited extent. No Scheme object is ever destroyed. The reason that implementations of Scheme do not run out of storage is that they are permitted to reclaim the storage occupied by an object if they can prove that the object cannot possibly matter to any future computation. Other languages in which most objects have unlimited extent include C#, Java, Haskell, most Lisp dialects, ML, Python, Ruby, and Samlltalk.

Implementations of Scheme must be properly tail-recursive. This allows the execution of an iterative computation in constant space, even if the iterative computation is described by a syntactically recursive procedure. Thus with a properly tail-recursive implementation, iteration can be expressed using the ordinary procedure-call mechanics, so that special iteration constructs are useful only as syntactic sugar.

Scheme was one of the first languages to support procedures as objects in their own right. Procedures can be created dynamically, stored in data structures, returned as results of procedures, and so on. Other languages with these properties include Common Lisp, Haskell, ML, Ruby, and Samlltalk. One distinguishing feature of Scheme is that continuations, which in most other languages only operate behind the scenes, also have "first-class" status. Furst-class continuations are useful for implementing a wide variety of advanced control constructs, including non-local exits, back-tracking, and coroutines.

In Scheme, the argument expressions of a procedure call are evaluated before the procedure gains control, whether the procedure needs the result of the evaluation or not. C, C#, Common Lisp, Python, Ruby, and Smalltalk are other languages that always evaluate argument expressions before invoking a procedure. This is distinct from by lazy-evaluation semantics of Haskell, or the call-by-name semantics of Algol 60, where an argument expression is not evaluated unless its value is needed by the procedure.

Scheme's model of arithmetic provides a rich set of numerical types and operations on them. Furthermore, it distinguishes exact and inexact number object. Essentially, an exact number object corresponds to a number exactly, and an inexact number object is the result of a computation that involved rounding or other errors.


Expressions

The most important elements of Scheme code are expressions. Expressions can be evaluated producing a value. The most fundamental expressions are literal expressions like #t, 23. Compound expressions are formed by placing parentheses around their subexpressions. The first subexpression identifies an operation; the remaing subexpression are operands to the operation, like (+ 23 42), (+ 14 15 (* 23 42)). As these examples indicate, compound expressions in Scheme are always written using the same prefix notation. As a consequence, the parentheses are needed to indicate structure. Consequently, "superfluous" parentheses, which are often permissible in mathematical notation and also in many programming languages, are not allowed in Scheme. As in many other languages, whitespace (inluding line endings) is not significant when it separates subexpressions of an expression, and can be used to indicate structure.

The syntax of Scheme code is organized in three levels. The **lexical syntax** that describes how a program text is split into a sequence of lexemes. The **datum syntax** formulated in terms of the lexical syntax, that structures the lexeme squence as a sequence of syntactic data, where a syntactic datum is a recursively structured entity. The **program syntax** formulated in terms of the read syntax, imposing further structure and assigning meaning to syntactic data.



The formal syntax for Scheme is written in an extended BNF. Non-terminals are written using angle brackets. All spaces in the grammar are for legibility. <Empty> stands for the empty string. The following extensions to BNF are used to make the description more concise: <thing>* means zero or more occurrences of <thing>, and <thing>+ means at least one <thing>. Some non-terminal names refer to the Unicode scalar values of the same name: <character tabulation> (U+0009), <linefeed> (U+000A), <carriage return> (U+000D), (line tabulation> (U+000B), <form feed> (U+000C), <carriage return> (U+000D), <space> (U+0020), <next line> (U+0085), <line separator> (U+2028), and <paragraph separator> (U+2029).

The **lexical syntax** determines how a character sequence is split into a sequence of lexemes, omitting non-significant portions such as comments and whitespace. The character sequence is assumed to be text according to the Unicode standard. Some of the lexemes, such as identifiers, representations of number objects, strings etc., of the lexical syntax are syntactic data in the datum syntax, and thus represent objects. Besides the formal account of the syntax, this section also describes what datum values are represented by these syntactic data.


``` dnf
<letter>
  : a...z
  | A...Z
<digit>
  : 0...9
<hex-digit>
  : <digit>
  | a...f
  | A...F
<digit2>
  : 0...1
<digit8>
  : 0...7
<digit10>
  : <digit>
<digit16>
  : <hex-digit>
<hex-scalar-value>
  : <hex-digit>+
<lexeme>
  : <identifier>
  | <boolean>
  | <number>
  | <character>
  | <string>
  | (
  | )
  | [
  | ]
  | #(
  | #vu8(
  | '
  | `
  | ,
  | ,@
  | .
  | #'
  | #`
  | #,
  | #,@
<delimiter>
  : (
  | )
  | [
  | ]
  | "
  | ;
  | #
  | <whitespace>
<whitespace>
  : <character-tabulation>
  | <linefeed>
  | <line-tabulation>
  | <form-feed>
  | <carriage-return>
  | <next-line>
  | <any-character-whose-category-is-Zs-Zl-Zp>
<line-ending>
  : <linefeed>
  | <carriage-return>
  | <carriage-return> <linefeed>
  | <carriage-return> <next-line>
  | <next-line>
  | <line-separator>
<comment>
  : ; <all-subsequent-character-up-to-a-line-ending-or-paragraph-separator>
  | #; <interlexeme-space> <datum>
  | #!r6rs
  | <nested-comment>
<nested-comment>
  : #| <comment-text> <comment-cont>* |#
<comment-text>
  : <character-sequence-not-containg-#|-or-|#>
<comment-cont>
  : <nested-comment> <comment-text>
<atmosphere>
  : <whitespace>
  | <comment>
<interlexeme-space>
  : <atmosphere>*
<identifier>
  : <initial> <subsequent>*
  | <peculiar-identifier>
<initial>
  : <constituent>
  | <special-initial>
  | <inline-hex-escape>
<constituent>
  : <letter>
  | <any-character-whose-unicode-scalar-value-is-greater-than-127-and-whose-category-is-Lu-Ll-Lt-Lm-Lo-Mn-Nl-No-Pd-Pc-Po-Sc-Sm-Sk-So-Co>
<special-initial>
  : !
  | $
  | %
  | &
  | *
  | /
  | :
  | <
  | =
  | >
  | ?
  | ^
  | _
  | ~
<inline-hex-escape>
  : \x<hex-scalar-value>;
<subsequent>
  : <initial>
  | <digit>
  | <any-character-whose-category-is-Nd-Mc-Me>
  | <special-subsequent>
<special-subsequent>
  : +
  | -
  | .
  | @
<peculiar-identifier>
  : +
  | -
  | ...
  | -> <subsequent>*
<boolean>
  : #t
  | #T
  | #f
  | #F
<character>
  : #\<any-character>
  | #\<character-name>
  | #\x<hex-scalar-value>
<character-name>
  : nul
  | alarm
  | backspace
  | tab
  | linefeed
  | newline
  | vtab
  | page
  | return
  | esc
  | space
  | delete
<string>
  : " <string-element>* "
<string-element>
  : <any-character-other-thenn-"-or-\>
  | \a
  | \b
  | \t
  | \n
  | \v
  | \f
  | \r
  | \"
  | \\
  | \x<hex-scalar-value>;
  | \<intraline-whitespace> <line-ending> <intraline-whitespace>
<intraline-whitespace>
  : <character-tabulation>
  | <any-character-whose-category-is-Zs>

<number>
  : <num2>
  | <num8>
  | <num10>
  | <num16>
<numR>
  : <prefixR> <complexR>
<complexR>
  : <realR>
  | <realR> @ <realR>
  | <realR> + <urealR> i
  | <realR> - <urealR> i
  | <realR> + <naninf> i
  | <realR> - <naninf> i
  | <realR> + i
  | <realR> - i
  | + <nrealR> i
  | - <urealR> i
  | + <naninf> i
  | - <naninf> i
  | + i
  | - i
<realR>
  : <sign> <urealR>
  | + <naninf>
  | - <naninf>
<urealR>
  : <uintegerR>
  | <uintegerR> / <uintegerR>
  | <decimalR> <mantissa-width>
<naninf>
  : nan.0
  | inf.0
<decimal10>
  : <uinteger10> <suffix>
  | . <digit10>+ <suffix>
  | <digit10>+ . <digit10>* <suffix>
  | <digit10>+ . <suffix>
<uintegerR>
  : <digitR>+
<prefixR>
  : <radixR> <exactness>
  | <exactness> <radixR>
<suffix>
  : <empty>
  | <exponent-marker> <sign> <digit10>+
<exponent-marker>
  : e
  | E
  | s
  | S
  | f
  | F
  | d
  | D
  | l
  | L
<mantissa-width>
  : <empty>
  | | <digit10>+
<sign>
  : <empty>
  | + 
  | -
<exactness>
  : <empty>
  | #i
  | #I
  | #e
  | #E
<radix2>
  : #b
  | #B
<radix8>
  : #o
  | #O
<radix10>
  : <empty>
  | #d
  | #D
<radix16>
  : #x
  | #X

```



Basic types

Scheme programs manipulate objects, which are also referred to as values. Scheme objects are organized into sets of values called types. Note: As Scheme is latently typed, the use of the term type in this report differs the use of the term in the context of other languages, particularly those with manifest typing.

**Booleans** A boolean is a truth value, and can be either true or false. In Scheme, the object for "false" is written #f. The object for "true" is written #t. Any Scheme value can be used as a boolean value for the purpose of a conditional test. In this test, all values count as true except for #f.

``` dnf
<boolean>
  : #t
  | #f
  | #T
  | #F
```

**Numbers** Scheme supports a rich variety of numerical data types, including objects representing integers of arbitrary precision, rational numbers, real numbers, complex numbers, and inexact numbers of various kinds.

其中有理数（rational）包括整数和分数（即有限小数和无限循环小数），实数（real）包括有理数和无理数（即无限不循环小数），复数（complex）由实数和虚数组成。

Number objects are organized as a corresponding tower of subtypes defined by the predicates number?, complex?, real?, rational?, and integer?. It is useful to distinguish between number objects that are known to correspnd to a number exactly, and those number objects whose computation involved rounding or other errors. For example, index operations into data structures may need to know the index exactly, as may some operations on polynomial coefficients in a symbolic algebra system. On the other hand, the results of measurements are inherently inexact, and irrational numbers may be approximated by rational and therefore inexact approximations. In order to catch uses of numbers known only inexactly where exact numbers are required, Scheme explicity distinguishes exact from inexact number objects. This distinction is orthogonal to the dimension of type.

A number object is exact if it is the value of an exact numberical literal or was derived from exact number objects using only exact operations. Exact number objects correspond to mathematical numbers in the obvious way. Conversely, a number object is inexact if it is the value of an inexact numerical literal, or was derived from inexact number objects, or was derived using inexact opeerations. Thus inexactness is contagious. 

Exact arithmetic is reliable in the following sense. If exact number objects are passed to any of the arithmetic procedures described below and an exact number object is returned, then the result is mathematically correct. This is generally not true if computations involving inexact number objects because approximate methods such as floating-point arithemtic may be used, but is is the duty of each implementation to make the result as close as practical to the mathematically ideal result.

The procedures listed below must return the mathematically correct exact result provided all their arguments are exact.

``` lisp
+ - * max min abs numerator denominator gcd
lcm floor ceiling truncate round rationalize
real-part imag-part make-rectangular
```

The procedures listed below must return the correct exact result provided all their arguments are exact, and no divisors are zero.

``` lisp
/ div mod div-and-mod
div0 mod0 div0-and-mod0
```

Moreover, the procedure expt must return the correct exact result provided its first argument is an exact real number object and its second argument is an exact integer object. The general rule is that the generic operations return the correct exact reault when all of their arguments are exact and the result is mathematically well-defined, but return an inexact result when any argument is inexact. Exceptions to this rule include sqrt, exp, log, sin, cos, tan, asin, acos, atan, expt, make-polar, magnitude, and angle, which may (but are not required to) return inexact results even when given exact arguments.

One general exception to the rule above is that an implementation may return an exact result despite inexact arguments if that exact result would be the correct result for all possible substitutions of exact arguments for the inexact ones. An example is (* 1.0 0) which may return either 0 (exact) or 0.0 (inexact).


A fixnum is an exact integer object that lies within a certain implementation-dependent subrange of the exact integer objects. Likewise, every implementation must designate a subset of its inexact real number objects as flonums, and to convert certain external representations into flonums. Note that this does not imply that an implementation must use floating-point representations.

Implementations of Scheme must support number objects for the entire tower of subtypes. Moreover, implementations must support exact integer objects and exact rational number objects of practically unlimited size and precision, and to implement certain procedures so they always return exact results when given exact arguments. "Practically unlimited" means that the size and precision of these numbers should only be limited by the size fo the avilable memory. 

Implementations may support only a limited range of inexact number objects of any type, subject to the requirements of this section. For example, an implementation may limit the range of the inexact real number objects (and therefore the range of inexact integer and rational number objects) to the dynamic range of the flonum format. Furthermore tha gaps between the inexact integer objects and rationals are likely to be very large in such an implementation as the limits of this range are approached.

An implementation may use floating point and other approximate representation stragegies for ineaxct numbers. This report recommands, but does not require, that the IEEE floating-point standards be followed by implementations that use floating-point representations, and that implementations using other representations should match or exceed the precision achievable using these floating-point standards.

In particular, implementations that use floating-point representations must follow these rules. A floating-point result must be represented with at least as much precision as is used to express any of the inexact arguments to that operation. Potentially inexact operations such as sqrt, when applied to exact arguments should produce exact answers whenever possible. However, this is not required. If, on the other hand, an exact number object is operated upon so as to produce an ineaxt result, and if the result is represented in floating point, then the most precise floating-point format available must be used; but if the result is represented in some other way then the representation must have at least as much precision as the most precise floating-point format available.

It is the programmer's responsibility to avoid using inexact number objects with magnitude or significand too large to be represented in the implementation.

Some Scheme implementations, specifically those that follow the IEEE floating-point standards, distinguish special number objects called positive infinity, negative infinity, and NaN. Positive infinity is regarded as an inexact real (but not rational) number object that represents an indeterminate number greater than the numbers represented by all rational number objects. Negative infinity is regarded as an inexact real (but not rational) number object that represents an indeterminate number less than the numbers representated by all rational numbers. A NaN is regarded as an inexact real (but not rational) number object so indeterminate that it might represent any real number, including positive or negative infinity, and might even be greater than positive infinity or less than negative infinity.

Some Scheme implementations, specifically those that follow the IEEE floating-point standards, distinguish between number objects for 0.0 and -0.0, i.e., positive and negative inexact zero. This report will sometimes specify the behavior of certain arithmetic operations on these number objects. These specification are marked with "if -0.0 is distinguished" or "implementations that distinguish -0.0".

> ChezScheme supports the full set of Scheme numeric datatypes, including exact and inexact integer, rational, real, and complex numbers. A variety of reprentations are used to support these datatypes. **Fixnums** represent exact integers in the fixnum range, The length of a string, vector, or fxvector is constrained to be a fixnum. **Bignums** represent arbitrary-precision exact integers outside of the fixnum range. **Ratnums** represent arbitrary-precision exact rational numbers. Each ratnum contains an exact integer (fixnum or bignum) numerator and an exact integer denominator. Rations are always reduced to lowest terms and never have a denominator of one or a numerator of zero. **Flonums** represent inexact real numbers. Flonums are IEEE 64-bit floating-point numbers. Since flonums cannot represent irrational numbers, all inexact real numbers are actually rational, although they may approximate irrational quantities). **Exact complexnums** represent exact complex numbers. Each exact complexnum contains an exact rational (fixnum, bignum, or ratnum) real part and an exact rational imaginary part. **Inexact complexnums** represent inexact complex numbers, Each inexact complexnum contains a flonum real part and a flonum imaginary part. Most numbers can be represented in only one way; however, real numbers are somtimes represented as inexact complex numbers with imaginary component equal to zero.









**Characters** Scheme characters mostly correspond to textual characters. More precisely, they are isomorphic to the scalar values of the Unicode standard. 

``` dnf
<character>
  : #\<any-character>
  | #\<character-name>
  | #\x<hex-scalar-value>
<character-name>
  : nul
  | alarm
  | backspace
  | tab
  | linefeed
  | newline
  | vtab
  | page
  | return
  | esc
  | space
  | delete
<hex-digit>
  : 0...9
  | a...f
  | A...F
<hex-scalar-value>
  : <hex-digit>+
<delimiter>
  : (
  | )
  | [
  | ]
  | "
  | ;
  | #
  | <whitespace>
<whitespace>
  : <character-tabulation>
  | <linefeed>
  | <line-tabulation>
  | <form-feed>
  | <carriage-return>
  | <next-line>
  | <any-character-whose-category-is-Zs-Zl-Zp>
```

Characters are represented using the notation #\<any-charater> or #\<character-name> or #\x<hex-scalar-value>. A <any-character> must be followed by a <delimiter> or by the end of the input. This rule resolves various ambiguous cases involving named characters, requiring, for example, the sequence of characters #\space to be interpreted as the space character rather than as the character #\s followed by the identifier pace. Note the #\newline notation is retained for backward compatibility. Its use is deprecated, #\linefeed should be used insteed.


**Strings** Strings are finite sequences of characters with fixed length and thus represent arbitrary Unicode texts. 

``` dnf
<string>
  : " <string-element>* "
<string-element>
  : <any-character-other-thenn-"-or-\>
  | \a
  | \b
  | \t
  | \n
  | \v
  | \f
  | \r
  | \"
  | \\
  | \x<hex-scalar-value>;
  | \<intraline-whitespace> <line-ending> <intraline-whitespace>
<intraline-whitespace>
  : <character-tabulation>
  | <any-character-whose-category-is-Zs>
<line-ending>
  : <linefeed>
  | <carriage-return>
  | <carriage-return> <linefeed>
  | <carriage-return> <next-line>
  | <next-line>
  | <line-separator>
```

Any other character (not list above) in a string after a backslash is a syntax violation. Except for a <line-ending>, any character outside of an escape sequence and not a douiblequote stands for itself in the string literal. A <line-ending> that does not follow a backslash stands for a linefeed character. For example:

``` lisp
"\x61;bc"      => "abc"

"a\
bc"            => "abc"

"a
bc"            => "a\nbc"
```





**Symbols** A symbol is an object representing a string, the symbol's name. Unlike strings, two symbols whose names are spelled the same way are never distinguishable. Symbols are useful for many applications; for instance, they may be used the way enumerated values are used in other languages.

'23                       => 23
'#t                       => #t
'(1 2 3)                  => (1 2 3)
'#(1 2 3)                 => #(1 2 3)
'(+ 23 42)                => (+ 23 42)
'(define (f x) (+ x 42))  => (define (f x) (+ x 42))

This facilitates writing programs that operate on Scheme source code, in particular interpreters and program transformers.




Pairs and lists, Vectors, Procedures


复合数据（Datum syntax）

The datum syntax describes the syntax of syntactic data in terms of a sequence of <lexeme>s, as defined in the lexical syntax. Syntactic data include the lexeme data described in the previous section as well as the following constructs for forming compound data: pairs and lists, eclosed by ( ) or [ ]; vectors; and bytevectors.

``` dnf
<datum>
  : <lexeme-datum>
  | <compound-datum>
<lexeme-datum>
  : <boolean>
  | <number>
  | <character>
  | <string>
  | <symbol>
<symbol>
  : <identifier>
<compound-datum>
  : <list>
  | <vector>
  | <bytevector>
<list>
  : (<datum>*)
  | [<datum>*]
  | (<datum>+ . <datum>)
  | [<datum>+ . <datum>]
  | <abbreviation>
<abbreviation>
  : <abbrev-prefix> <datum>
<abbrev-prefix>
  : '
  | `
  | ,
  | ,@
  | #'
  | #`
  | #,
  | #,@
<vector>
  : #(<datum>*)
<bytevector>
  : #vu8(<u8>*)
<u8>
  : <any-number-representing-an-exact-integer-in-0...255>
```

The most general notation for Scheme pairs as syntactic data is the "dotted" notation (<datum1> . <datum2>) where <datum1> is the representation of the value of the car field and <datum2> is the representation of the value of the cdr field. A more streamlined notation can be used for list. The elements of the list are simply enclosed in parentheses and separated by spaces. The empty list is represented by (). For example, following are equivalent notations for a list of symbols.

``` list
(a b c d e)
(a . (b . (c . (d . (e . ())))))
```

Note that the datum above is just a textual representation, not a expression that can evaluate a value. Using ' can transform the textual representation to the expression. For example:

``` lisp
'23                       => 23
'#t                       => #t
'foo                      => foo
'(1 2 3)                  => (1 2 3)
'#(1 2 3)                 => #(1 2 3)
'(+ 23 42)                => (+ 23 42)
'(define (f x) (+ x 42))  => (define (f x) (+ x 42))
```

The ' in the previous examples are not needed for representations of number objects or booleans. The 'foo is a symbol with name "foo", foo is a identifier has a real value associated with in it. And (1 2 3), #(1 2 3) are only textual representations, '(1 2 3) and '#(1 2 3) are literal expressions. The ' in procedure call and definition are also not needed.

Syntactic data is also called external representations, and Scheme's libarary provides the get-datum and put-datum procedures for reading and writing syntactic data, converting between their textual representation and the corresponding objects. A syntacitic datum can be used in a program to obtain the corresponding datum value using quote.

Syntactic data in Sheme source code are called forms. A form nested inside another form is called a subform. Consequently, Scheme's syntax has the property that any sequence of characters that is a form is also a syntactic datum representing some object. This can be load to confusion, since it may not be obvious out of context whether a give sequence of characters is intended to be a representation of objects or the text of a program. It is also a source of power, since it facilitates writing programs such as interpreters or compilers that treat programs as objects or vice versa.





Semantic concepts

A Scheme program consists of a top-level program together with a set of libraries, each of which defines a part of the program connected to the others through explicitly specified exports and imports. A library consists of a set of export and import sepcifications and a body, which consists of definitions, and expressions. A top-level program is similar to a library, but has no export specifications.

A top-level program specifies an entry point for defining and running a Scheme program. A top-level program specifies a set of libraries to import and code to run. Through the imported libraries, whether directly or through the transitive closure of importing, a top-level program defines a complete Scheme program. A top-level program is a delimited piece of text, typically a file, that has the following form. And a <top-level-body-form> is either a <definition> or an <expression>.

``` lisp
(import <import-spec> ...)
<top-level-body-form> ...
```

A library definition must have the following form:

``` lisp
(library <library-name>
  (export <export-spec> ...)
  (import <import-spec> ...)
  <library-body>)
```

Within the body of a library or top-level program, an identifier may name a kind of syntax (called keyword or syntactic keyword), or it may name a location where a value can be stored (called variable). Certain forms are used to create syntactic abstractions and to bind keywords to transformers for those new syntactic abstractions, while other forms create new locations and bind variables to those locations. Collectively, these forms are called binding constructs. Some binding constructs take the form of definitions, while others are expressions.

Expressions that bind variables include the lambda, let, let*, letrec, letrec*, let-values, and let*-values forms from the base library. Of these, lambda is the most fundamental. Expressions that bind keywords include the let-syntax and letrec-syntax forms. A define form is a definition that creates a variable binding, and a define-syntax form is a definition that creates a keyword binding.

A Scheme expression can evaluate to an arbitrary finite number of values. These values are passed to the expression's continuation. Not all continuations accept any number of values. For example, a continuation that accept the argument to a procedure call is guaranteed to accept exactly one value. The effect of passing some other number of values to such a continuation is unspecified. The call-with-values procedure makes it possible to create continuations that accept specified numbers of return values.


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

While definitions are not expressions, but it has similar syntactic structure as compound expressions. At the purely syntactical level, both are forms, and form is the general name for a syntactic part of a Scheme program. In particular, 23 is a subform of the form (deinfe x 23).

Scheme variables bound by definitions or let or lambda expressions are not actually directly to the objects specified in the respective bindings, but to locations containing these objects. The contents of these locations can subsequently be modified destructively via assignment.

(let ((x 23))
  (set! x 42)
  x)              => 42

The expression (set! x 42) is an assignment, saying "replace the object in the location referenced by x with 42". Thus, the previous value of x, 23, is replaced by 42.



Procedures

Definitions can also be used to define procedures:

(define (f x)
  (+ x 42))
(f 23)             => 65

A procedure is, slightly simplified, an abstraction of an expression over objects. In the example, the definition defines a procedure called f. The expression (f 23) is a procedure call, meaning, roughly, "evaluate (+ x 42) (the body of the procedure) with x bound to 23". Procedures are objects, they can be passed as values to other procedures. In fact, many predefined operations of Scheme are provided not by syntax, but by variables whose values are procedures. The + operation, for example, which receives special syntactic tratment in many other languages, is just a regular identifier in Scheme, bound to a procedure that adds number objects. The same holds for * and many others.

Procedure definitions are not the only way to create procedures. A lambda expression creates a new procedure as an object, with no need to specify a name. For example, ((lambda (x) (+ x 42)) 23), the lambda expression defines a anonymous procedure, and the entire expression is a procedure call.

Whereas (+ 23 42), (f 23), and ((lambda (x) (+ x 42)) 23) are all examples of procedure calls, lambda and let expressions are not. This is because let, even though it is an identifier, is not a variable, but is instead a syntactic keyword. A form that has a syntactic keyword as its first subexpression obeys special rules determinted by the keyword. The define identifier in a definition is also a syntactic keyword. Hence, definitions are also not procedure calls. The rules for the lambda keyword specify that the first subform is a list of parameters, and the remaining subforms are the body of the procedure. In let expressions, the first subform is a list of binding specifications, and the reaming subforms constitute a body of expressions. 

Procedure calls can generally be distinguished from these specific forms by looking for a syntactic keyword in the first position of an form. If the first position does not contain a syntactic keword, the expression is procedure call. So-called identifier macros allow creating other kinds of special forms, but are comparatively rare. The set of syntactic keywords of Scheme is fairly small, with usually makes this task fairly simple. It is possible, however, to create new bindings for syntactic keywords. 

Many of the special forms specified in this report can be translated into more basic special forms. For example, a let expression can be translated into a procedure call and a lambda expression.

(let ((x 23) (y 42)) (+ x y))      => 65
((lambda (x y) (+ x y)) 23 42)     => 65

Special forms like let expressions are called derived forms because their semantics can be derived from that of other kinds of forms by a syntactic transformation. Some procedure definitions are also derived forms. The following two definitions are equivalent:

(define (f x) (+ x 42))
(define f (lambda (x) (+ x 42)))

In Scheme, it is possible for a program to create its own derived forms by binding syntactic keywords to macros:

(define-syntax def
  (syntax-rules ()
    ((def f (p ...) body)
     (define (f p ...)
       body))))

(def f (x)
  (+ x 42))

The define-syntax construct specifies that a parenthesized structure matching the pattern (def f (p ...) body), where f, p, and body are pattern variables, is translated to (define (f p ...) body). Thus, the def form appearing in the example gets translated to (define (f x) (+ x 42)). The ability to create new syntactic keywords makes Scheme extremely flexible and expressive, allowing many of the features built into other languages to be derived forms in Scheme.



Continuations

Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. For example, informally the continuation of 3 in the expression (+ 1 3) adds 1 to it. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly.

The call-with-current-continuation procedure allow Scheme programmers to do that by creating a procedure that reinstates the current continuation. This procedure accepts a procedure, calls it immediately with an argument that is an escape procedure. This escape procedure can then be called with an argument that becomes the result of the call to call-with-current-continuation. That is, the escape procedure abandons its own continuation, and reinstates the continuation of the call to call-with-current-continutation.

In the following example, an escape procedure representing the continuation that adds 1 to its argument is bound to escape, and then called with 3 as an argument. The continuation of the call to escape is abandoned, and instead the 3 is passed to the continuation that adds 1.

(+ 1 (call-with-current-continuation
       (lambda (escape)
         (+ 2 (escape 3)))))
=> 4

An escape procedure has unlimited extent. It can be called after the continuation it captured has been invoked, and it can be called multiple times. This makes call-with-current-continuation significantly more powerful than typical non-local control constructs such as exceptions in other languages.



Libraries

Scheme code can be organized in components called libraries. Each library contains definitions and expressions. It can import definitions from other libraries and export definitions to other libraries. The following library called (hello) exports a definition called hello-world, and imports the base library and the simple I/O library.

(library (hello)
  (export hello-world)
  (import (rnrs base)
          (rnrs io simple))
  (define (hello-world)
    (display "hello world")
    (newline)))

A Scheme program is invoked via a top-level program. Like a library, a top-level program contains imports, definitions and expressions, and specifies an entry point for execution. Thus a top-level program defines, via the transitive closure of the libraries it imports, a Scheme program. The following top-level program obtains the first argument from the command line via the command-line procedure. It then opens the file using open-file-input-port, and calls to get-bytes-all procedure to obtain the contents of the file as binary data. It then uses put-bytes to output the contents of the file to standard output.

#!r6rs
(import (rnrs base) (rnrs io ports) (rnrs programs))
(put-bytes (standard-output-port)
           (call-with-port (open-file-input-port (cadr (command-line)))
                           get-bytes-all))






