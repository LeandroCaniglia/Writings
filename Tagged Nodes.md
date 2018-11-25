# Extending the Smalltalk Syntax
Leandro Caniglia - November 17, 2018

**Story 3:** *Tagged Nodes*
--

What do you do when you have to include JSON in a Smalltalk method? Something like this?
```
jsonCoordinates
  ^'{
      "latitude": 48.858093,
      "longitude": 2.294694
    }'
```
In other words, do you represent JSON data with plain strings? Wouldn't it be nice to improve this? What if the compiler knew that this `String` should conform to a specific syntax? What if the Smalltalk browser knew how to format JSON strings? Or even color them for the sake of readability?

Before jumping to JSON strings, let's step back and think of other cases that might be similar. Have you ever represented HTML as a plain `String`? What about CSS or even JavaScript? I'm sure you have faced situations where you inlined foreign code as a plain `String` in a method, keeping that information in your head rather than in the method, where it should naturally belong? Want to change this? Ok. Let's do it.

Where to start? Here is the roadmap:

1. Consider the introduction of _tags_ for inlining foreign scripts.
2. Introduce a new subclass of `LiteralNode` named `TaggedNode`.
3. Consider the introduction of foreign parsers such as a `JsonParser`.
4. Introduce a new class of AST node named `ForeignNode`.
5. Combine all the features above.

Task 1: Smalltalk tags?
--

Before making a decision for tags, let's see which other options we have. In order to inline foreign scripts, we must tell the Smalltalk parser how to delimit them. There are several delimiters in Smalltalk:

- White space
- Single and double quotes
- Parenthesis and brackets (both square and curly)

Can we think of any other? Backticks are tempting. The problem is that they would only work for a single semantics. For example. Say we decide to delimit JSON using backticks; how would we delimit HTML or CSS or JavaScript or Assembler or C if in the future we have a need for any of these?

We need flexibility and that's why tags are a good choice.

Using tags we will be able to inline foreign code like This
```
jsonCoordinates
  ^<json>
    {
      "latitude": 48.858093,
      "longitude": 2.294694
    }
  </json>
```
