The Trickle programming language will be an FRP system similar to
[Reflex](https://reflex-frp.org/). It is designed for GUI programming and
for front-end, though it might also be deployed in back-end situations. It
will be dynamically typed with regards to data, but statically typed
with regards to whether a variable (or a function return) is a widget,
dynamic, event, function, or pure value, referred to for right now
as which "monad" a value is in (until a new term can potentially be
found). The monad will be indicated with a system of Perl-esque sigils.

Even though it is not yet implemented, going forward I will use the
present tense to describe it.

# Data

Trickle is dynamically typed with regards to data.

## Strings

Strings are UTF-8 strings. They are written with `""` or `''`.  Either
allows `\` to be used to escape. `'` only recognizes `'` and `\` as a
special characters requiring escape. `"` also does string interpolation,
either by writing a variable (such as `"Hello $foo"`) or by writing
`{}` (`"Hello {$foo}"`) with an arbitrary expression.

## Symbols and Labelled Tuples

Symbols are strings that are optimized for comparison and matching.
They are sigiled with a `#`, and are either written in
`#lower_snake_case` or are surrounded with `'` or `"`. Booleans are
represented as symbols (`#true` and `#false`). The `"` notation
can be used to convert strings to symbols.

Symbols can be suffixed with a parenthesized list of data to create
labelled tuples, which serve in a similar role to variants of sum
types. Typical examples include `#some(3)` vs `#none`, or
`#tree(#tree(#node(2), #node(3)), #node(4))`.

## Lists

Lists are written with comma separation and `[]`, e.g. `[1, 2, 'hi']`.

## Maps

Maps have arbitrary key and value types. They are written with `{}`
but with `:` or `->` between key and value. If `:` is used, the key
is a bare word taken as a symbol, otherwise it's an expression.

## Numbers

Numbers are either integers or floating point. Floating points are
written with decimal places (`1.3`) and integers without them (`3`).

# Monads and Variables

All variables must be distinguished based on what monad they are in.
Variable names must be `lower_snake_case`, and function names must be
`UpperCamelCase`. Variables cannot contain functions, and function sigils
indicate what value they return. Different monads are also different
namespaces, so `!foo`, `$foo`, `@foo` and `%foo` are all potentially
distinct variables.

All variables are block-scoped. Each variable can only be bound once
per scope. Binding is done with `=` if the right hand side is the same
monad, or `<-` if the right hand side is a widget.  Variables can refer
to each other higher or lower in the code, but recursive pure values
are not allowed. Sigils and monads can be nested, which requires
`\` to tell the monad checker to treat the expression as nestable.

## Pure

Pure variables use `$` as their sigil. They can only hold pure data,
rather than events, dynamics, widgets or functions.

```
$foo = #true
$bar = if $foo { 3 } else { 4 }
```

When pure functions and operators are applied to other types,
they are automatically lifted. So `!foo + 3` returns an event
that takes the values from the `!foo` event and adds `3` to each
of them.

Unlike other sigils, `$` is not nestable. It's sort of like the
absence of a monad. Pure values may not contain any monadic values,
and monads are assumed to contain no pure values.

## Events

An event represents a stream of values. Their sigil is `!`.  They cannot
be externally triggered. ! when applied to a dynamic value gets the
event of when that value is updated. `!never` is a built-in constant
that never updates.

Events can be transformed by pure functions, by optional functions,
or by a special syntax that takes *when* the event fires from another
event, but replaces the value with a constant or dynamic value.
That syntax looks like this:

```
!event1 = !<3> !event0
!event2 = !<@value> !event1
```

By default, using a dynamic value inside an event samples the dynamic
value. To create a nested event-of-dynamic situation, you must use `\`:

```
!@dyn_event = !<\@value> !basic_event
```

## Dynamic

Dynamic variables use `%` as their sigil. They change over the course
of the program, but not through explicitly writing to them. The program
when binding a dynamic can get it from:

* A source outside of Trickle (e.g. the current value of a text box)
* `%` applied to a pure value, so that `%2` never updates
and always has the value `2`.
* `@%` can also be applied to an event, but an initial value must be
specified, in angle brackets before the event. This automatically
promotes the value to a widget.
* A pure expression applied to another dynamic results in a modified dynamic

```
fn @%Count(!event) {
    !new_number = !<@dyn + 1>!event
    %dyn <- @%<0>!new_number
    return %dyn
}

%count <- @%Count(!some_event)   // %count contains how many times event fired
```

## Optional

Optionality is indicated by the sigil `?`. Optional-returning functions
can use `guard` to return an absent value based on boolean, as a free-standing
statement, or they can use `?` as a postfix operator to propagate
optionality. `guard` can also be used in a function returning `!` to
indicate that the event does not happen, and `?` when the value is
expected to be an event filters events where the value is not present.

Optional values can be assigned into regular values with the prefix `*`
operator, which causes them to be turned into `#none` or `#some(foo)`,
lowered into the pure realm. Postfix `?` on a regular value interprets
it as an optional by the same convention. This allows you to write
e.g. `*?foo == #none` as a test.

Doing normal operations on an optional propagates the optionality,
e.g. `?foo + 3` adds `3` if the value is present, but continues if
the value is absent.

## Widget

A widget block allows `<-` as well as `=`, which unpacks a widget
value. The top level of a Trickle program is a widget block, but
from a non-widget block, no widget blocks can be created. Widget
blocks are introduced with `@{` instead of `{`, or a function returning
`@` automatically introduces it.

It also allows raw statements with side effects of plopping down UI
elements. Putting a widget value as a statement by itself plops down
all its UI elements.

Widget expressions can also be raw HTML, which is quoted with backticks
and introduced with `@`.  HTML expressions have interpolations like
double-quoted strings, and for attributes like on-click, it can be used
to define events. Quotes are not used for these backwards definitions,
and need not be used for interpolation for attributes.

This shows a button whose label is a count of how many times it's been
clicked (assuming the definition above for `@%Count`).

```
%count <- @%Count(!clicked)
@`<button onclick=!clicked>%count</button>`
```
