# Minimal Scheme implemented in Rust

> *Do It, Do It Again, and Again, and Again ...*  
> &emsp; — *The Little Schemer* by Friedmann and Felleisen

![Lisp cycles XKCD #297: "Those are your father's parentheses. Elegant weapons for a more... civilized age."](https://imgs.xkcd.com/comics/lisp_cycles.png)

(source https://xkcd.com/297/)

**rusch** implements the following procedures:

- `(car pair)` returns the first element, and `(cdr pair)` returns the second element (tail) of the *pair*.
- `(cons obj1 obj2)` creates pair where *obj1* is car and *obj2* is cdr. `(list obj1 obj2 ...)` is the same as
  `(cons obj1 (cons obj2 (cons obj3 ...)))`.
- `(define name value)` assigns *value* to a *name* in the current environment. `(set! name value)` if the *name* exists
  in the current or enclosing environment, it sets it to the *value*, otherwise, it assigns *value* to a *name* in the
  current environment.
- `(lambda (arg1 arg2 ...) expr1 expr2 ...)` defines a [lambda expression] (*aka* function). There is also an equivalent,
shorter way of writing `(define name (lambda (arg1 arg2 ...) expr1 expr2 ...))` as `(define (name arg1 arg2 ...) expr1 expr2 ...)`.
- `(let ((name1 value1) (name2 value2) ...) expr1 expr2 ...)` evaluates *expr1*, *expr2*, ... in the local environment,
  with *name1*, *name2*, ... variables present; returns the result of evaluating the last *exprN* expression.
  `let*` is like `let`, but the *arg1*, *arg2*, ... arguments are evaluated sequentially, from left to right,
  and the following arguments can depend on the preceding.
- `(if condition if-true if-false)` and `(cond (test1 expr1) (test2 expr2) ...)` conditionals with special `else`
condition always evaluating to `#t`, e.g. `(cond (else 'yay))`.
- `(begin expr1 expr2 ...)` evaluates *expr1*, *expr2*, ..., returns the result of evaluating the last *exprN* expression.
- `(quote expr)` or `'expr` returns *expr* without evaluating it. While `quote` is commonly used for constructing lists,
  [it is not the same] as `list`. `(quasiquote expr)` or `` `expr`` works like `quote`, but parts of the expression can
  be evaluated using `(unquote expr)` or `,expr`, for example `` `(2 + 2 = ,(+ 2 2))`` will evaluate to `(2 + 2 = 4)`.
- `(eval expr)` does the opposite to `quote` by evaluating *expr*, e.g. `(eval '(+ 2 2))` returns `4` rather than the
`(+ 2 2)` list.
- `(eq? obj1 obj2)` compares if two objects are equal, `equal?` is just an alias for it.
- Logical `(not obj)`, `and`, and `or`, e.g. `(and obj1 obj2 ...)`.
- Arithmetic operators `+`, `-`, `*`, `/`, e.g. `(+ x1 x2 ...)`, and `%` for remainder [as defined in Rust].
  Those procedures promote integers to floats if any of the arguments is a float. Division `/` always promotes arguments
  to floats, for integer division use `//`.
- Numerical comparison operators `<`, `=`, `>`, e.g. `(< x1 x2 ...)`.
- Checkers for the [disjoint types]: `pair?`, `number?`, `boolean?`, `string?`, `symbol?`, `procedure?`, and
  other checkers: `integer?`, `float?`, `null?` (empty list) and `nil?` (null value).
- `->int` and `->float` transformations from any numeric types to integers and floats.
- `(string expr ...)` converts *expr*s to string, `(display expr ...)` prints them,
  and `(error expr ...)` raises exceptions with *expr*s as a message.
- `reverse` can be used to reverse a list.

Comments begin with `;` and everything that follows, from the semicolon until the end of the line, is ignored.

The design of this implementation is similar to [the one in Go] and it was described in more detail in the [blog post].

 [the one in Go]: https://github.com/twolodzko/gosch
 [blog post]: https://twolodzko.github.io/posts/lisp-in-rust.html
 [lambda expression]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_6.html#SEC30
 [it is not the same]: https://stackoverflow.com/questions/34984552/what-is-the-difference-between-quote-and-list
 [disjoint types]: https://www.cs.cmu.edu/Groups/AI/html/r4rs/r4rs_5.html#SEC23
 [as defined in Rust]: https://stackoverflow.com/questions/31210357/is-there-a-modulus-not-remainder-function-operation
