# Ish-v2 language

## Installation

Using `make` creates `./interpreter`. `make test` runs all the tests.

## Files

- `src/Main.hs`, the main file;
- `src/Syntax.hs`, the AST of the program;
- `src/Parser.hs`, the definition of the `Parser` type + some other stuff;
- `src/Lexer.hs`, the lexer;
- `src/Layout.hs`, tools for indentation-sensitive parsing;
- `src/ParserDefs.hs`, parsers of the AST elements and parser directives (more on that later);
- `src/VM.hs`, the definitions of an evaluation environment ("a virtual machine", if you will);
- `src/Eval.hs`, interpreter _proper_, i.e. a set of functions which parse AST into `Eval` instances;
- `src/Intrin.hs`, intrinsic types and functions;
- `src/Ish.hs`, definition and lifting of `Parser` and `Eval` into a common monad.

## Syntax

I will write it in an approximate form, not Backus-Naur. For concrete examples, see the test suite and `src/Intrin.sh` for `future`.

```
<program> ::=
<toplevel>
...
<toplevel>

<toplevel> ::=
  <stmt>
  | <directive>

<directive> ::= #[op <precedence> <type:infix{l,r,n},{pre,post}fix> <op>]

% Simple expressions
<expr> ::=
  <integer literal>
  | <string literal>
  | *                              % Kind signature
  | rec                            % Reserved word for recursive call
  | <variable>                     % Starts with letter, then only alphanum
  | <expr>(<expr>, ...)            % Function application
  | <op> <expr>                    % Prefix op
  | <expr> <op> <expr>             % Infix op
  | <expr> <op>                    % Postfix op
  | fn <expr>(<expr>, ...)

% Function declaration
<expr> ::=
fn [[<expr>]] (<expr>, ..., <expr>)
   <stmt>
   ...
   <stmt>

<stmt> ::=
  let {<variable> [: <expr>] = <expr>}, ... % Variable definition
  | <variable> = <expr>                     % Variable assignment
  | <expr>                                  % Lone-expr eval (includes fn app)
  | break
  | continue
  | return [<expr>]
  | future <variable>: <expr>               % Forward declaration

<stmt> ::=
if <expr>
  <stmt>
  ...
  <stmt>
[else
  <stmt>
  ...
  <stmt>]

<stmt> ::=
while <expr>
  <stmt>
  ...
  <stmt>
```

## Feature table

|                        Feature                        | Present? |
| :---------------------------------------------------: | :------: |
|                   **For 15 points**                   |
|            Three types: int, bool, string             |   Yes    |
|         Literals, arithmetic and comparisons          |   Yes    |
|            Variables, assignment operator             |   Yes    |
|                       Printing                        |   Yes    |
|                     `while`, `if`                     |   Yes    |
| Functions and procedures (without nesting), recursion |   Yes    |
|      At least two methods for passing parameters      |    No    |
|    Read-only variables and their use in for-loops     |    No    |
|                   **For 20 points**                   |
|                Static scope resolution                |   Yes    |
|       Handling errors, such as division by zero       |   Yes    |
|      C-like functions, i.e. not only procedures       |   Yes    |
|                **For up to 30 points**                |
|                     Static typing                     |   Yes    |
|        Arbitrarily nested functions/procedures        |   Yes    |
|         Records or structs or arrays or lists         |    No    |
|         Tuples and Python-like destructuring          |    No    |
|                `break` and `continue`                 |   Yes    |
|            First-class functions, closures            |   Yes    |
|         Generators and syntactic sugar for it         |    No    |

### Functions and procedures

In the version from 9 VI, recursion wasn't fully implemented; it is now.

### Static typing

Aside from just static typing, a rudimentary system for type inference is present, to not have to add return types for functions returning functions, which would be very awkward. In such a case, `rec` keyword cannot be used, because then we'd have problems with for example:

```
let f = fn(x: int)
  return f(x)
```

which I didn't particularly want to delve into.

### Overloading

Overloading is available, for every type (i.e. not just functions), since functions are on the same level as "normal" values. Of particular interest is name lookup for `f(x1, ..., xn)`: in this case, `x1`...`xn` are looked up (without any type constraint), whereafter `f` is looked up with type constraint `fn ?(t1, ..., tn)`. Closest matching name-value is chosen.

### Custom operators

One can also declare custom operators via `#[op {prec} {type} {op}]` directive.

## Test suite

We _were_ supposed to, I think, encode them with numbers etc. but I didn't feel I could map these features one-to-one; the names should be sufficient, and in any case here is the list of all of them:

### Bad

- `bad/cannot_use_rec.ish` - example of `rec` being undefined when type declaration is unannotated.
- `bad/div_by_zero.ish` - division by zero error.
- `bad/fn_infer_error.ish` - an example where a function cannot have an infered type, for example because two different types are returned.
- `bad/keyword_as_var.ish` - (self-evident).
- `bad/layout.ish` - an example of a layout error.
- `bad/let_type.ish` - a declared variable with an explicit type annotation must be assigned a value of a proper type (usually it's inferred from the type of the right hand side).
- `bad/no_name.ish` - where a variable that wasn't defined is referenced.
- `bad/no_op.ish` - where an undefined operator is used.
- `bad/no_var_with_type.ish` - where a variable with a given name _was_ defined, but we need one of a different type.
- `bad/num_of_args.ish` - where a function is defined but we pass an incorrect number of arguments. Formally it's just a lookup error.
- `bad/static_bind.ish` - where a function references a variable that wasn't defined at that point but will be defined when the function is called.

### Good

- `good/closures.ish` - (self-evident).
- `good/custom_op.ish` - where we declare and instantiate a custom operator.
- `good/fn_params.ish` - where we showcase passing functions as parameters to other functions.
- `good/overload.ish` - where we use overloading (in a somewhat contrived fashion, but still).
- `good/recur.ish` - where we use recursive functions (namely Fibonacci number and factorial).
- `good/static_bind.ish` - where we provide a working/compiling example of the static lexical binding.
- `good/type_infer.ish` - where we show type inference in action.
- `good/while.ish` - where we use `while` in combination with `break` and `continue`.

## Remarks

The error printing is pretty bad, and from what I tested sometimes the operators don't want to work, but I wanted to keep it as it was in the 9 VI version. For that matter, pretty much everything, aside from the test suite and this document, is the same as it was, except for recursion which I couldn't help but finish.
