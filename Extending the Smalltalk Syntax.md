# Extending the Smalltalk Syntax?
*Adding Squeak Braces*
--

Does your dialect support Squeak Braces? To answer this question try to evaluate the following expression

`{Date today. 3 + 4}`

if you get an Array with the above two elements, today's date and 4, it does. Otherwise, you would get a compilation error. In the latter case you might be interested in extending the syntax of you dialect so to have what I call Squeak Braces.

Where to start? Here is the roadmap:

1. Find the hierarchy of classes that model parse tree nodes.
2. Add a new class for modeling brace arrays (let's call it `BraceNode` or `ArrayNode`)
3. Add an ivar to the class for holding the elements of the array (let's call it `elements`)
4. Add a method to the class for transforming the elements into a cascade node (see below)
5. Add all other required methods that nodes need to implement, especially those that message nodes have

Now let's see some hints on how to accomplish the tasks above:

1. Finding the AST hierarchy
--
The Abstract Syntactic Tree (a.k.a. AST) is the object that models the decomposition of a piece of source code into its constituents. This typically includes nodes for both the hole method and also its parts: literals, blocks, variables, etc. Therefore I would start by trying to find a class that includes `Literal` or `LiteralNode` in its name. From this class go to the top of the hierarchy and you will get the complete picture of the place where you will be working next

2. Adding the new class
--
This simple, just subclass from the root of the AST hierarchy a new class appropriately named. Say `BraceNode`.

3. Adding the required ivars
--
For sure we will need an ivar to keep the elements of the array. So, add the `elements` ivar. We will likely add one more ivar later though.

4. Transforming the node into a cascade node
--
The idea here is to add a new method, say `#asCascadeNode` whose job is to answer with the `CascadeNode` that results from the expression

`(Array with: n)
   at: 1 put: (elements at: 1);
   ...;
   at: n put: (elements at: 2);
   yourself`

where `n` is `elements size`. To do this, you need to find the `CascadeNode` in the AST hierarchy and become familiar with it so you can create one instance of it for the method to return.

5. Add other required methods
--
Typically, AST nodes implement the `#acceptVisitor:` message for supporting the *Vistor* pattern's double dispatching mechanism. It's implemenation is straightforward:

`acceptVisitor: aVisitor
   aVisitor acceptBraceNode: self`

You also need to find all visitors that you will need to enhance. To discover them look for implementors of visiting messages in the AST hierarchy such as `#visitCascadNode:`, etc. For other messages, take inspiration from the other nodes, especially, from `CascadeNode`.
