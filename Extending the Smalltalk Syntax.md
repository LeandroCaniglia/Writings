# Extending the Smalltalk Syntax
**Story 1:** *Adding Squeak Braces*
--

Does your dialect support Squeak Braces? To answer this question try to evaluate the following expression

`{Date today. 3 + 4}`

if you get an `Array` with the above two elements, today's date and 7, it does. Otherwise, you would get a compilation error. In the latter case you might be interested in extending the syntax of your dialect so to have what I call Squeak Braces.

Where to start? Here is the roadmap:

1. Find the hierarchy of classes that model parse tree nodes.
2. Add a new class for modeling brace arrays (let's call it `BraceNode` or `ArrayNode`)
3. Add an ivar to the class for holding the elements of the array (let's call it `elements`)
4. Add a method to the class for transforming the elements into a cascade node (see below)
5. Add all other required methods that nodes need to implement, especially those that message nodes have

Now let's see some hints on how to accomplish the tasks above:

Task 1: Finding the AST hierarchy
--

The Abstract Syntactic Tree (a.k.a. AST) is the object that models the decomposition of a piece of source code into its constituents. This typically includes nodes for both the whole method and also its parts: literals, blocks, variables, etc. Therefore I would start by trying to find a class that includes `Literal` or `LiteralNode` in its name. From this class go to the top of the hierarchy and you will get the complete picture of the place where you will be working next

Task 2: Adding the new class
--

This simple, just subclass from the root of the AST hierarchy a new class appropriately named. Say `BraceNode`.

Task 3: Adding the required ivars
--

For sure we will need an ivar to keep the elements of the array. So, add the `elements` ivar. We will likely add one more ivar later though.

Task 4: Transforming the node into a cascade node
--

The idea here is to add a new method, say `#asCascadeNode` whose job is to answer with the `CascadeNode` that results from the expression

```
(Array with: n)
  at: 1 put: (elements at: 1);
  ...;
  at: n put: (elements at: 2);
  yourself
```

where `n` is `elements size`. To do this, you need to find the `CascadeNode` in the AST hierarchy and become familiar with it so you can create one instance of it for the method to return.

Task 5: Add other required methods
--

Typically, AST nodes implement the `#acceptVisitor:` message for supporting the *Vistor* pattern's double dispatching mechanism. It's implemenation is straightforward:

```
acceptVisitor: aVisitor
   aVisitor acceptBraceNode: self`
```

You also need to find all visitors that you will need to enhance. To discover them look for implementors of visiting messages in the AST hierarchy such as `#visitCascadeNode:`, etc. Writing each of the required implementor of `#visitCascadeNode:` is also straightforward:

```
visitBraceNode: aBraceNode
   self visitCascadeNode: aBraceNode asCascadeNode
```

For other messages, take inspiration from the other nodes, especially, from `CascadeNode`.

Improvements
--

Once you have all of this complete and tested, you may want to improve your implementation a little bit. For instance, some occurrences of these Squeak Braces are just literals. One example would be

`{3. $a. {'hello' 'world'}}`

This is actually equivalent to:

`#(3. $a. #('hello' 'world'))`

However, our implementation would work as if we had written

```
(Array new: 3)
  at: 1 put: 3;
  at: 2 put: $a;
  at: 3 put: (
    (Array new: 2)
      at: 1 put: 'hello';
      at: 2 put: 'world';
      yourself);
  yourself
```

which sends 9 messages instead of none! To avoid this waste what we can do is to give literal arrays a special treatment. Here is how.

Task 6
--

At the top of the AST hierarchy add the method `#isLiteral` returning `false` (this might or might not be there already). Now repeat the same for `LiteralNode` except that this time answer with `true`. Finally, add the `#isLiteral` method to `BraceNode` on the lines of:

```
isLiteral
  ^elements conform: [:e | e isLiteral]
```

given that the `elements` of our `BraceNode` are themselves instances of AST nodes, this closes the circle.

Task 7
--

Next, add the `#asLiteralNode` method to `BraceNode` on the lines of:
```
asLiteralNode
  ^LiteralNode new
    value: self literal;
    start: self star;
    stop: self stop
```

So, the only piece that is missing is the `#literal` message. Here it is:

```
literal
  ^elements collect: [:e | e literal]
```

where

```
LiteralNode >> #literal
  ^self value
```

Taks 8
--

Finally, modify your implementations of `#visitBraceNode:` method like this
```
visitBraceNode: aBraceNode
  aBraceNode isLiteral
    ifTrue: [self visitLiteralNode: aBraceNode asLiteralNode]
    ifFalse: [self visitCascadenode: aBraceNode asCascadNode]
```

Depending on the internals of your system, some additional tweaks might be required. Just make sure your testing coverage is high enough. I almost forgot! In **Task 3** I said that we would likely add another ivar to `BraceNode`, remember? Well, I was thinking in a cache for the result of `#asCascadeNode` or `#asLiteralNode`, depending on the case. This will help to preserve the identity of the `BraceNode` substitute, should the translation be required more than once. No need to do this in advance though. Just keep it in mind.
