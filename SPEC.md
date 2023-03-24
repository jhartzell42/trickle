# An Idea for a PL

1. Duck-typed (for now)
2. Garbage-collected (how does this even work?)
3. Bytecode interpreted
4. Implicitly multi-tasked (and multi-threaded?)

# Basic Semantics

Data types:
* Numbers
* Strings
* Lists
* Dictionaries (called records)
* `SomeIdentifier("values", 3, 4)`
    * Can be 0-ary and therefore just symbols
* Streams: Generators/live blocks of code
    * Serve as handles on running tasks
    * Many "functions" are live stream globals
    * Bidirectional

Identifier types:
* Symbols/tuple constructors are `UpperCamelCase`
* Variables are `snake_case`
* `@lower_snake` for built-in streams and code blocks
    * Basically imagine its short for `PROLOG.`
* Streams have bare names in `<-` expressions
    * Are otherwise normal variables
    * Same namespace
* Code blocks can only be instantiated

Basic operators:
* Normal arithmetic operators
* Tuple construction per above, as both constructor and pattern
* `[1, 2, 3]` for list literals
* `[ x: 1, y: 2, [string1 + string2]: 3 ]` for dictionary literals and patterns
    * `x:` means key is `x`
    * `[x]:` means `x` is a string whose value should be the key
    * Ordering is preserved, is list of `KeyValue(_,_)` tuples
* `[]` for indexing lists
    * Lists can also be used as queues
* `.x` for indexing records with bareword `x` interpreted as field name
    * `.[x]` to interpret `x` as expression to get field name as string
* `REMOVE x[y]` to remove a specific item from list or record
    * Returns item
* `<-` and `->` to move one item of data between streams
* `$` converts a value or pattern to sender or receiver of single item
    * Value yields exactly itself
    * Pattern can set exactly itself as well
* `(<- stream)` evaluates to one item of data out of stream
    * Parentheses unnecessary where unambiguous
* `<--` and `-->` to stream indefinitely as new task
* Prefix '&' creates an aliased stream
    * By default, streams move
* Prefix `*` copies a stream, restarting it
    * Identity on non streams
    * Can be dangerous!
    * Otherwise, assigning a stream aliases the stream
        * Streams are only things that can be aliased!
            * This seems weird -- streams should move by default?
            * Maybe a "duplication" operator?
            * Many streams are "pure", aliasing is fine
        * Everything else absolutely by value/copy
            * Including closure capture
* Pattern operators:
    * Prefix `NUMBER` constrains a pattern to a number
    * Prefix `STRING` constrains a pattern to a string
    * Prefix `STREAM` constrains a pattern to a stream
    * `_` discards values
* `%` is given special meaning within `{` ... `}` block

Capture:
* `STREAM` and `FOR&` create closures that capture variables
    * Content variables are captured by copy
    * Streams are captured by move by default
        * Are we sure?
    * Capture list in first line for streams:
        * `*` to capture by copy, `&` to capture by alias, `!` for by move

Basic statements:
* `IMPORT` reads a library file into a dictionary
    * Can use `as` to name dictionary different
    * Can use `into` to fill an existing dictionary
        * e.g. `into PROLOG` to fill up `@`s
    * `PROLOG` is auto-read-in, and normally referred to by `@`
        * Contains built-ins
* Python-style significant whitespace
* `pattern = value`
    * If pattern fails, end current task
* Expressions can stand on their own as statements
* `REPEAT:`
    * Actually expression that evaluates to whatever is passed to `BREAK`
    * If nothing passed to break, can't be used as expression
        * No nulls! Use symbols if you must! But no built-in meaning for `None`.
* `BREAK` + optional expression
* `CONTINUE`
* `EXIT`
    * End current task
* `FOR $variable <- stream:`
    * Loop until stream is closed
    * Expression again returning value of `BREAK`
* `FOR& $variable <- stream:`
    * Does not wait for previous to run next
* `IF $variable <- stream:` ... `ELSE:`
    * Run inner block if stream has more to give.
    * If stream is closed in that direction, run `ELSE` block if any
* `IF pattern = value` ... `ELSE`
    * Conditional patterns
    * Very basic match
* These are actually expressions:
    * `IF cond:` ... `ELIF cond:` ... `ELSE:`
    * `MATCH value:` ... `ON pattern:` ... `ON pattern:`
    * `STREAM`, `{` ... `}`
* DONE WRITING stream
    * Terminates any `FOR` loops waiting for the stream
    * Further reads from other side will cause task to end
    * Further writes to this stream cause task to end
* DONE READING stream
    * Promise never to read from again
    * Further reads from this stream cause task to end
    * Further writes from other side cause task to end
* DONE ALL stream
    * Both! :-)
    * Nixes the task on the other side of this stream. Good fun!
    * Especially useful for using with `<--`

When you have a running stream, you can send it messages:

```
foo <- $Bar(3, 4, 5)            # Send value to `foo`
$result <- foo                  # Wait for value from `foo`, store in `result`
result = <- foo                 # Equivalent
result <- foo                   # Wait for value from `foo`, send to `result`
result <- $foo                  # Send `foo` stream to result
$result <- foo <- $Bar(3, 4, 5) # Send value to `foo`, get value back, put in `result`
```

Alternatively, you can say "all messages from `foo` go to `bar`"
by using `<--` or `-->`. This launches a task that runs indefinitely
in background:

```
bar <-- foo
foo --> bar
```

These return a combined task that has the input of `foo` and the output
of `bar`, in case you want to attach more tasks to them. But, if that
input or output is unnecessary, you can also just leave them run.

You can have multiple simultaneous attempts to send and receive from
a stream. Each message coming out of a stream goes to one receiver.
If a task sends to and receives from a stream in a single statement,
it gets first priority getting the output after the input is sent. This
allows streams to be used in a query-and-reply way.

Figure out a non-crazy way to generalize that type of flow control!
Hmm... In general, when a stream accepts a value from a task, the next
values it sends goes to that task, so long as that task then listens
without performing any other stream operations besides listening.
The instant that task performs another stream operation, it finds
another task to send that value to.

Or maybe that's too strong a promise and we want a special send-and-receive
exception.

In either case, note that if you send multiple things to a stream in
a task, you could be interleaving with another task. If that makes for
race conditions, just alias streams less often.

Streams can be defined as running code.  From inside the stream
definition, you send outputs by sending to `OUT` and input by querying
`IN`. Yes, you can use `$` to duplicate these streams to other places.

Both of them are closed on the inactive side, so writing to `IN` or
reading from `OUT` ends the task. Reaching the end of the block of
code also ends the task. Ending the task closes the stream completely,
so that further attempts to read or write to it may fail.

```
incrementing_stream = STREAM:
    @console <- "Hello"
    FOR $in <- IN:
        OUT <- $(in + 1)
incrementing_stream = *incrementing_stream     # Outputs "hello" again
@console <- incrementing_stream <- $5          # Outputs "6"
incrementing_stream <-- [0, 1, 2, 3, 4, 5, 6]
@console <-- incrementing_stream  # outputs lines counting from 1 through 7
```

If you write `{` ... `}` instead of `STREAM`, you can have implied the
`FOR $in <- IN`, except for with the special variable `%`, and the last
statement is an expression interpreted as sent to `OUT`. So:

```
incrementing_stream = { % + 1 }
```

Within `{` and `}`, `BEGIN:` and `END:` can label statements that
run before and after the loop, making this an exact equivalent of
`incrementing_stream` above:

```
incrementing_stream = {
    BEGIN: @console <- "Hello"
    % + 1
}
```

Note that in this abbreviated notation, `IN` and `OUT` are still
accessible. The `=` is optional in assignments that just have a `{}`-block
on the right-hand side:

```
incrementing_stream { % + 1 }
```

`%123` is equivalent to `%[123]`, and `%foo` to `%.foo`, so if `%` is
a list or a record, it's easier to access. Additionally, if the
entire body of the `{}` is `ON`-clauses, it'll read in a `MATCH %`:

```
to_string {
ON STRING str: str
ON NUMBER num: <- @number_to_string <- num # Or something like that
# ...
}
```

# Standard Library

## `@console`

Reads lines from standard input as strings. Outputs strings to console,
stringifies anything else before it outputs it.

## `@to_string`

Replies with value as a string.

## `@type`

Replies with value's type, per `MATCH`:

```
{
ON STRING _: String
ON NUMBER _: Number
ON STREAM _: Stream
ON TUPLE _: Tuple
ON LIST _: List
ON RECORD _: Record
}
```

## `@duplicate`

Takes a stream, returns `Pair(a, b)` where send sides are aliased,
but receive sides duplicate rather than choosing one.

## `@merge`

Takes list of streams, returns a stream where sends send to all and
receives receive from the first one to send you something.

## `@hold`

Gives you a stream that holds. The stream it gives you has this property:
If you send it something, it will immediately give you that until you
send it something else. If you have not yet sent it something, it sends
you nothing.

Attempted implementation:

```
hold = $STREAM: # `$` + value makes constant output stream
    $value <- IN:
    process = OUT <-- $value
    for $value <- IN:
        STOP process
        process = OUT <-- $value
```

Another attempted implementation:

```
hold = $STREAM:
    process = None
    for $value <- IN:
        IF Some(process) = process:
            STOP process
        process = Some(OUT <-- $value)
```
