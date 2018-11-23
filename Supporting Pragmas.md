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
However, not all of them support other types of pragmas, as Pharo does. So, let's see what it would take adding support for them to our dialect.

Where to start? Here is the roadmap:

1. Find the Smalltalk Parser in your system
2. In the Smalltalk Parser find the place where primitives are parsed
3. Modify the code that parses primitive declarations to recognize pragmas as well
4. Find a place in the CompiledMethod where to store the pragama annotation

Now let's see some hints on how to accomplish the tasks above:

Task 1: Finding the Smalltalk Parser
--
Look for selectors like `parse`, `parse:`, `parserMethod:`, `parseLiteral:`, etc. In some dialects the entry point to parsing services is the `Compiler`. You can also look for implementors of `parserClass`, if any.
