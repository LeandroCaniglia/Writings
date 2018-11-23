# Extending the Smalltalk Syntax
Leandro Caniglia - November 22, 2018

**Story 2:** *Supporting Pragmas*
--

All Smalltalk dialects support _pragmas_. Here is one:
```
identityHash
  <primitive: Hash>
  ^0
```
However, not all of them support other types of pragmas, as Pharo does. So, let's see what it would take to add support for them to our dialect.

Where to start? Here is the roadmap:

1. Find the Smalltalk Parser in your system
2. In the Smalltalk Parser find the place where primitives are parsed
3. Modify the code that parses primitive declarations to recognize pragmas as well
4. Find a place in the `CompiledMethod` to store the pragama annotation

Now let's see some hints on how to accomplish the tasks above:

Task 1: Find the Smalltalk Parser
--
Look for selectors like `parse`, `parse:`, `parseMethod:`, `parseLiteral:`, etc. In some dialects the entry point to parsing services is the `Compiler`, so you can debug the initial steps required to compile any expression until you reach the parser. You can also check to see if there are implementors of `parserClass`.

Task 2: Find the parsing of primitive declarations
--
This is best accomplished debugging the compilation of any method with a primitive declaration. Just create a primitive method and debug its compilation. After some few clicks you should get to the parser method that distinguishes it as primitive.

Task 3: Parse the pragma
--
For this task I would recommend starting simple. There will be time for further sophistication. In this case simple means, assume the pragma is just one token, like in
```
methodWithPragma
  <ourPragma>
  self doSomething; etc.
```
The existing code should complain if the token that comes after `$<` is not `'primitive:'` (or friends, if any). And this is precisely where we need to branch, save the token as our pragma and (in this first attempt) save it in the `MethodNode` (whatever the name of this class happens to be). Note that there is no need to add a new ivar to `MethodNode`, we can save the token in the same ivar where primitives are held. After all, primitives are numbers while pragamas are not, so we will able to distinguish between them.

Don't forget to consume the closing `$>`, which should be the next token.

Task 4: Save the pragma in the method
--
This step is actually not required as we could always resort to the AST for getting the pragma. One could argue that it would be too slow to parse the entire method every time we want to access its pragma, should it have any. However, since pragams occur before any other statements, there is no need to build the entire AST. In fact, the only we need is a service in the parser that would stop parsing right before the parsing of sentences starts.

However, if you prefer avoiding any parsing, when a new `CompiledMethod` is compiled, the `MethodNode` should somehow inject the pragma in it. One way to do this is to add the pragma as the first literal. However, if we do this, how are we going to tell whether the first literal is a pragma or not.

There might be several tricks for this. For instance, in most dialects the `SmallInteger` `0` is never saved in the literal frame. The reason is that there are special bytecodes to operate with it, so the constant `0` doesn't need to go into the literal frame.

Therefore, we could add the pragma as the first literal, and then add `0` as the second. Thus, in order to check and retrieve the pragma of a method we would do the following:
```
CompiledMethod >> pragma
  literals := self literals.
  ^(literals size >= 2 and: [(literals at: 2) = 0])
    ifTrue: [literals at: 1]
```
