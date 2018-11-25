# Extending the Smalltalk Syntax
Leandro Caniglia - November 24, 2018

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
5. Process the body of the foreign script, according to its semantics.

Task 1: Smalltalk tags?
--

Before making a decision for tags, let's see which other options do we have. In order to inline foreign scripts, we must tell the Smalltalk parser how to delimit them. There are several delimiters already taken in Smalltalk:

- White space
- Single and double quotes
- Parenthesis and brackets (both square and curly)

Can we think of any other? Backticks are tempting. The problem is that they would only work for a single semantics. Say we decide to delimit JSON using backticks; how would we delimit HTML or CSS or JavaScript or Assembly or C, should the future bring a need for any of them?

We want flexibility and that's why tags are a good choice.

Using tags we will be able to inline foreign code like This
```json
jsonCoordinates
  ^<json>
    {
      "latitude": 48.858093,
      "longitude": 2.294694
    }
  </json>
```
And how do we make sure that tags do not confuse the Smalltalk parser? To answer this question think of all the places where `$<` is misplaced in regards to the Smalltalk syntax:

- On the right of assignments
- When a message argument is expected
- On the right of the return symbol

In other words, none of the following sequence of characters conforms to the Smalltalk syntax:

- `temp := <`..
- `3 + <`...
- `self msg: <`...
- `^<`...

See? Every potential syntax error is an opportunity for extending the syntax!

Of course, angle brackets `<...>` are already legal in the Smalltalk syntax as pragma delimiters. But pragmas are illegal when placed in assignments, arguments and returns. To be valid, they must start a Smalltalk statement. And this is precisely why we will forbid tags at the beginning of statements and restrict them to assignments, arguments and returns.

Task 2: Add the class for tagged nodes
--
A tagged node is a way of delimiting foreign code and as such it is a new type of literal. So, add a subclass of `LiteralNode` named `TaggedNode`. This subclass will add the `tag` ivar that will link its instances to their specific meaning.

As we depticted above, instances of `TaggedNode` need to be instantiated by the parser in the following four cases:

- When parsing an argument of a keyword message
- When parsing the argument of a binary message
- When parsing the expression of an assignment
- When parsing the expression of a return node

This means that we need to modify essentially four methods so that they now check whether the next character is `$<`. If it is not, the original code regains control. Otherwise, the code branches to a new method that will _scan_ the `TaggedNode` or fail (if there is no closing tag, etc.).

I've used the verb _to scan_ because in order to form a `TaggedNode` we will need to scan the input at the character level (usually the parser deals with tokens provided by the scanner).

When scanning the opening tag we will need to read the input until `$>` is reached (issuing and error if it isn't). This will give us the value for the `tag` ivar of the `TaggedNode`. At this time we will also know that the closing tag should be `'/', tag`. So we can read the _body_ of the foreign code until `'</', tag ,'>'` is found (error if not).

At this point we are ready to

Task 3: Decide how to process the foreign script
--
Now we have access to the body of the `TaggedNode`. What do we do with it? Well, this depends on the semantics we want to give it. In the case of JSON, for instance, it would be enough to parse it using a JSON parser, and then format it using a JSON writer. We could also paint it with colors and emphases, so to make it look great in our environment.

In other cases, such as the one where the foreign code is Assembly, we could decide to go a step further and compile it into machine code. This will bring two capabilities: (1) Parsing and formatting/painting the Assembly source code and (2) Making the node answer with the corresponding machine code when the method is executed.

There are many other possibilities. In the case of JavaScript or any other programming language, we could decide to execute it on top of Smalltalk (at least up to some extent, this should be feasible).

On the other end of our wide horizon of possibilities there is one that consists in doing nothing, i.e, simply keeping the body as a `String` with no special semantics. This is useful for experimentation. For instance, if you plan to write a parser for inlining VBA code, before embarking in such a project, you might want to see how the tagged code would look. You will need to format it yourself and will have no coloring available. However, it will bring a secondary benefit: you will not have to worry about duplicating embedded quotes (in VBA the quote is the comment separator).

The last case is the simpler one. Still, it requires the introduction of a new kind of literal node, which we will call `StringNode`. So, instead of keeping the body in the `TaggedNode` as a `String`, we will create a `StringNode` with the body as its `value` and will keep this new node as the `value` of the `TaggedNode`. This indirection will provide the flexibility we need for making free use of `TaggedNode`.

Note also that while making these decisions you should keep in mind that sometimes it is not necessary to implement a parser of the entire specification of the foreign language. For instance, if you will only deal with C-Types and C-Structures, you don't need to parse arbitrary C, you just need to parse these declarations. The same might be true with other languages. Usually you will only inline a limited subset of them. The key here is to create the machinery that will make further enhancements easier and consistent.

Task 4: The foreign node
--
So far we have discussed two new nodes: `TaggedNode` and `StringNode`, both subclasses of `LiteralNode`. The ivar `tag` in `TaggedNode` holds the node's tag string. Where we have to be careful is in deciding the contents of the `value` ivar because this is where the semantics enters the game.

Since we are planning for support of different languages, we will need a global `Registry` of available parsers/compilers. For instance, the package that loads the JSON parser will be able to register the `<json>` tag with the corresponding parser. Similarly for `<html>`, `<css>`, `<asm>`, `<js>`, `<vba>`, etc.

In this way, when the `TaggedNode` receives the `#body:` message with the foreign code as the argument, it will be able to enter the `Registry` with its `tag` and get the corresponding `parser` from there. If there is none, the `TaggedNode` will resort to `StringNode`, passing it the body and keeping this node in its `value` ivar.

```smalltalk
TaggedNode >> body: aString
  value := Registry
    at: tag
      ifPresent: [:p | ForeignNode new parser: p]
      ifAbsent: [StringNode new].
  value value: aString
```
The `ForeignNode` will have two ivars: `parser` and `ast`. The latter is computed as follows:

```smalltalk
ForeignNode >> value: aString
  ast := parser parse: aString
```

Task 5: Compile/Process
--
Now that we have all the pieces in place we can use them to, at least, format and/or color the foreign code. This is simply achieved by asking the `ForeignNode` its `ast` or its formatted/colored representation. You could also provide more advanced featrues such as the ones we have mentioned in **Task 3**, or even take advantage of yet another technique that we will discuss in the next story which is _Hybrid Compilation_.
