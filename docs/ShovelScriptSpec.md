<!-- -*- markdown -*- -->

# ShovelScript specification

## Introduction

ShovelScript is a simplified JavaScript, but it is neither a subset or
a superset of JavaScript. It tries to be a decent Scheme with infix
syntax.

This document assumes the reader knows how to program and is somewhat
familiar with JavaScript or Scheme.

## Statements

A ShovelScript program is made of statements. A statement is:

 * a variable declaration,
 * a non-local return statement,
 * an assignment or
 * an expression.

All statements have values. In a sense, statements are a special class
of expressions (they have values, but they can't be used everywhere an
expression can be used - e.g. a statement is an invalid `if`
condition).

Values have types, variables are untyped. There are no implicit
conversions and the program execution stops if a value of the wrong
type is supplied to an operation. ShovelScript is therefore a
dynamically and strongly typed language.

See the section 'Types' below for a description of the available
types.

### Variable Declarations

Syntax:

    var <variable-name> = <expression>

Where `<variable-name>` is an identifier denoting the introduced
variable. Identifiers are made of at-signs, underscores, letters and
digits and must not start with a digit. Variable names cannot be
reserved words (see the 'Reserved Words' section below) or required
primitive names (see the 'Required Primitives' section below).

`<expression>` is a ShovelScript expression (see the 'Expressions'
section below).

The value of this statement is `<expression>`.

### Non-Local Returns

Syntax:

    return <block-name> <return-value>

Where `<block-name>` is an expression evaluating to a string which is
interpreted as the name of the block to return from (see section
'Named Blocks' below) and `<return-value>` is an expression which
evaluates to the value returned.

### Assignments

Syntax:

    <variable-name> = <new-value>

Where `<variable-name>` is an expression identifying a memory location
and `<new-value>` is the value to be stored in the memory location.

`<variable-name>` can be an identifier naming a variable, an element
of an array or string (e.g. `array[index]` or
`string[character_index]`) or a dotted expression
(e.g. `f(x).address.street`) or a combination of all of the above
(e.g. `a.b(x)[i][j].k.l`).

The value of this statement is `<new-value>`.

An assignment to an undeclared variable is an error.

## Expressions

An expression can be:

 * a closure expression (starting with keyword `fn`),
 * an alternative expression (starting with keyword `if`),
 * a named block (starting with keyword `block`) or
 * a 'simple expression'.

### Closure Expressions

Syntax:

    fn <argument-name> <statement>

or

    fn (<argument-name>) <statement>

or

    fn (<arg-1>, <arg-2>, ... <arg-n>) <statement>

Where `<argument-name>`, `<arg-1>` etc. are identifiers and
`<statement>` is a statement that is used to calculate the return
value for a function invocation (the value of the statement is the
returned value).

`<statement>` can get or set any of the variables that are accessible
where the closure is defined (lexical scoping, indefinite extent).

### Alternative Expressions

Syntax:

    if <condition> <then-statement>

or

    if <condition> <then-statement>> else <else-statement>

`<condition>` is an expression that is evaluated. If it is the boolean
value `true`, the value of the alternative is the value of
`<then-statement>`, otherwise if `<condition>` evaluates to the
boolean `false`, the value of the alternative is the value of
`<else-statement>>` (if the `<else-statement>` is missing, the
alternative expression has the void value `null`). If `<condition>`
evaluates to a non-boolean the program's execution stops with an
error.

### Named Blocks

Syntax:

    block <block-name> <statement>

Where `<block-name>` is an expression evaluating to a string which
names the block. `<statement>` is a regular statement. If a `return`
statement is executing while evaluating `<statement>` and the block
name specified for `return` is equal to `<block-name>`, the block
exits immediately (regardless of how many function calls are on the
stack between the `block` declaration and the `return` statement). The
value of the block is either the value of `<statement>` or the return
value from a `return` statement that caused a non-local exit from the
block.

### 'Simple Expressions'

These are expressions obtained using operators, according to the
following precedence table:

    ()
    function call, array access, dot access
    unary -, unary !
    * / % ^ &
    + - |
    < > <= >= != ==
    &&
    ||

`()` is a parenthesized expression.

`!` is logical *not*.

`*` is multiplication, `/` is division (truncating if both operands
are integers), `%` is modulo (valid only if both operands are
integers), `^` is bitwise *xor*, `&` is bitwise *and*.


`+` and `-` are addition and subtraction. Subtraction is only valid
for numbers, but `+` works for numbers (addition) and strings or
arrays (concatenation). `|` is bitwise *or*.

The next row is for relational operators, which have the same meaning
as in C. `!=` is true if `==` is not, and `==` is true if both
arguments are `null`, they are both strings or numbers or booleans and
are equal. `==` is false if only one operand is null or the operands
are both strings or numbers or booleans but are not equal. `==` causes
the program to exit in any other case (e.g. values of different types
are compared).

`&&` and `||` are short-circuiting boolean *and* and *or*,
respectively. 'Short-circuiting' means that `t1 && t2` is in fact
evaluated as `if t1 t2 else false` and `t1 || t2` is in fact evaluated
as `if t1 true else t2`. Since ShovelScript stops program execution if
the wrong types are supplied, `&&` can be used to write guards:

    if isString(t1) && t1 == 'string-expression'

The above protects against crashes in case the value of `t1` is not a
string. The short-circuiting `&&` means that the second operand is
only evaluated if the first is true.

An expression can also be a literal number (integer or floating
point), a literal string (enclosed in single or double quotes, with
inside quotes escaped with a backspace), a literal boolean (`true` or
`false`), or `null`.

An expression can also be an unnamed block, which is a sequence of
statements enclosed by `{` and `}`. The value of the unnamed block is
the value of the last statement inside the block. An unnamed block
introduces a fresh scope for variables (variables defined in it are
only visible inside the block and inside closures defined inside the
block).

An expression can also be 

 * an identifier, 
 * an expression followed by `.` and another identifier (hash access), 
 * an expression followed by `(`, a possibly empty comma separated
   list of expressions and `)` (a function call),
 * an expression followed by `[`, an expression and `]` (array or hash
   access).

It is an error to access a hash value in the form `h.k` if there is no
key named `k` in hash `h`. An assignment to `h.k` is valid even if
there is no key `k` in hash `h`. `h[k]` is valid for both getting and
setting the key in variable `k` (in this last case the key is not `k`
itself, but the value of variable `k`).

Hash accesses can be done either via `[]` (e.g. `address['street']`)
or via `.` (e.g. `address.street`). Array accesses use only `[]`
(e.g. `vertex[1]`).

An expression can also be simply `context`, which evaluates to a hash
with two keys, `stack` and `environment`, containing string
representation of the stack trace and current environment at the
moment the `context` expression is evaluated.

## Comments
ShovelScript supports single-line comments, indicated by double slash `//`

```
   @f(5) // Call f()
```

Multi-line comments are not supported.

## Types

The following scalar types are available:

 * string,
 * integer (modulo 2^60),
 * double precision floating point,
 * boolean (`true` or `false`) and
 * void (with only one value, `null`).

All operations on integers trim the result to 60 bits.

The other types allowed are:

 * callables (primitives or ShovelScript closures),
 * arrays (elements can be any primitive type, including arrays) and
 * hashes (associative arrays, keys can be strings, values can have
   any ShovelScript type).
   
## Keywords

ShovelScript has the following reserved keywords:

    var if else fn return
    true false null block context

It is illegal to name a variable with one of these keywords. Hash
accesses of the form `h.k` are illegal if `k` is a reserved keyword.

## Required Primitives

It is illegal to name a variable the same as a required primitive.

### `pow`

Arguments: `base`, `exponent`

Description: Raises `base` to power `exponent` and returns the
result. Both arguments must be numbers. If one of the arguments is a
double float, the result is a double float, otherwise it is an
integer.

### `array`

Arguments: `element-1`, `element-2` etc.

Description: Creates an array from the arguments.

### `arrayN`

Arguments: `size`

Description: Creates an array with `size` elements, each initialized
to `null`. `size` must be an integer.

### `length`

Arguments: `array-or-string`

Description: Returns the number of elements in the array or the number
of characters in the string.

### `slice`

Arguments: `array-or-string`, `start-index`, `end-index`

Description: Returns a subarray or substring of `array-or-string`,
starting at `start-index` and ending before `end-index`. `start-index`
and `end-index` have to be integers. If `start-index` or `end-index`
is negative, the length of `array-or-string` is added to them before
extracting the slice. This way, one can specify extracting all the
elements but the last using `slice(arr, 0, -2)`.

### `push`

Arguments: `array`, `new-element`

Description: Adds `new-element` at the end of `array`. `array` must be an array.

### `pop`

Arguments: `array`

Description: Removes the last element in `array` and returns it. The
execution stops with an error if the array is empty. `array` must be
an array.

### `lower` and `upper`

Arguments: `string`

Description: `lower` creates a new string that is equal to `string`
with all letters converted to lower case. `upper` does the similar
conversion to upper case. The argument is not modified.

### `hash`

Arguments: `key-1`, `value-1`, `key-2`, `value-2`, ... `key-n`,
`value-n`

Description: Creates a hash from the keys and values provided. All
keys must be string expressions (not necessarily string literals).

### `keys`

Arguments: `hash`

Description: Returns an array containing the keys of hash
`hash`. `hash` must be a hash.

### `hasKey`

Arguments: `hash`, `key`

Description: `true` if `hash` has a key named `key`, `false`
otherwise. `hash` must be a hash and `key` must be a string.

### `delete`

Arguments: `hash`, `key`

Description: deletes `key` (and the associated value) from the hash.

### `utcSecondsSinceUnixEpoch`

Arguments: none

Description: What the name says. A large integer, the number of
seconds since January 1 1970 00:00 UTC.

### `decodeTime`

Arguments: `utcSeconds`

Description: Returns a hash with keys `second`, `minute`, `hour`,
`day`, `month`, `year` and `dayOfWeek` representing the decoded
`utcSeconds` (which must be an integer representing a date as the
number of seconds since the UTC Unix Epoch).

`second` and `minute` are between 0 and 59, `hour` between 0 and 23,
`day` is the day of the month, `month` is between 1 and 12, `year` is
the current 4-digit year, `dayOfWeek` is the day of the week (1 is
Monday, 7 is Sunday).

### `encodeTime`

Arguments: `timeHash`

Description: `timeHash` must be a hash like the one returned by
`decodeTime`. Returns the corresponding number of seconds since UTC
Unix Epoch.

### `isString`, `isHash`, `isBool`, `isArray`, `isNumber`, `isCallable`

Arguments: `expr`

Description: `true` if `expr` is of the type mentioned in the function
name, `false` otherwise.

### `string`

Arguments: `expr`

Description: Returns a string representing `expr`. Useful mostly for
converting scalars to strings.

### `stringRepresentation`

Arguments: `expr`

Description: Returns a string representing the ShovelScript code that
would create a data structure like `expr`. Unlike `string`, it walks
arrays and hashes and calls itself recursively to print their content.

It prints `[...callable...]` for callables and `[...loop...]` for
items that make `expr` a circular data structure.

Example:

    var a = array(1, 2, 3)
    a[1] = a
    stringRepresentation(a)

returns:

    array(1, [...loop...], 3)

### `parseInt`

Arguments: `string-expr`

Description: Returns the integer represented as a string by
`string-expr`. Execution stops if `string-expr` doesn't contain a
valid integer representation.

### `parseFloat`

Arguments: `string-expr`

Description: Similar to `parseInt`, but for double precision floats.

### `floor`

Arguments: `numeric-expression`

Description: Returns the largest integer smaller than
`numeric-expression`.

### `panic`

Arguments: `message`

Description: Finishes program execution with message `message`.

## User-Defined Primitives

A function name that starts with an 'at sign' denotes a user-defined
primitive (UDP). These are functions that are made available to the
ShovelScript by the process hosting Shovel. For instance,
`@getUserName(userId)` is a call to such a user-defined primitive and
`var fun = @udp` assigns a user-defined primitive to a variable.

UDPs need to handle their own host-language exceptions (there are no
exceptions in ShovelScript) and encode the encountered exceptional
situations in the value that they return.

UDPs cannot call ShovelScript functions (that would make break the
'interruptible' property of ShovelScript functions).
