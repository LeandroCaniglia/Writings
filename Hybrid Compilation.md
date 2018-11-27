# Extending the Smalltalk Syntax
Leandro Caniglia - November 26, 2018

**Story 4:** *Hybrid Compilation*

Have you ever heard of the idea the creators of Smalltalk had for allowing any class to choose its compiler? To provide support for this classes respond to the `#compiler` message before the actual compilation is attempted. Why then, this capability hasn't been exploited yet? Are you willing to investigate it further? OK, bear with me.

In Story 3 of this series we discussed how to inline `TaggedNodes` in a Smalltalk method. We mentioned several applications of this capability and took JSON as a basic example. Today we can take a similar approach and try to see how to do our exploration with JSON in mind. From there it will be fairly clear how to proceed in other cases. So, let's put ourselves this objective: compile the following method in our Smalltalk dialect:
```json
jsonCoordinates
  <json>
  ^{
      "latitude": 48.858093,
      "longitude": 2.294694
    }
```

Where to start? Here is the roadmap:

1. Discuss the introduction of pragmas for enabling foreign compilation.
2. Introduce the `HybridCompiler` class.
3. Generate the hybrid method when there are no arguments.
4. Introduce the `ParametricString` class.
5. Generate the hybrid method when there are arguments.

Task 1: Discussion
--

Let's start by noticing how our example above is slightly different from what we did in Story 3. Here `<json>` is not a _tag_ but a _pragma_ (there is no closing tag). We are using this pragma to make it clear that we will be using a foreign compiler.

A similar example with JavaScript
```javascript
canSimulateCase
	<js>
	$scope.canSimulateCase = function(c) {
		return !$scope.isProcessingCase(c)
	};
```

One difference with tagged nodes is that here the _entire method body_ is written in a foreign language. Why this variation is interesting? Because it will allow us to pass Smalltalk arguments to foreign methods. Like this:
```smalltalk
jsonCoordinates: lat longitue: long
  <json>
  ^{
      "latitude": #lat,
      "longitude": #long
    }
```
meaning that the foreign source code will be generated dynamically.

Note that I've used `$#` to mark what follows as an argument. We don't want to replace every occurrence of `'lat'` and `'long'` with the arguments; want we?, so we need to tell where we want the replacements to happen. The reason for using `$#` as a marker is that it presents (almost) no collision with foreign tokens.

Task 2: Hybrid Compiler
--
If we get back to our examples above, we will see that these methods have two parts: (1) a Smalltalk header including the pragma and (2) the foreign code. This accounts for hybrid compilation. We need to, at least, parse the beginning of the method to read the pragma that tells which compiler to pick, and then pass it the body. For doing all of this we will need the following class

```javascript
Object
	subclass: #HybridCompiler
	instanceVariableNames: 'source smalltalk foreing method'
	classVariableNames: ''
	poolDictionaries: ''
```

The `smalltalk` ivar is initialized to the Smalltalk compiler, and `foreign` with the compiler (or parser) associated to the method's pragma. When the `source` is set, the `smalltalk` compiler is used to read the pragma (`'json'` in our example). At this point the `Registry` (see **Story 3**) will provide us with the `foreign` parser. If there is no pragma or there is one which is not in the `Registry`, the compilation is entirely on `smalltalk`. The `method` ivar will hold the compilation result.

Task 3: Hybrid method
--

Once an instance of `HybridCompiler` has been initialized. It is time to compile the method. For now we will assume that there are no arguments (_unary_ case).
```
HybriCompiler >> compile
  | cm |
  foreign isNil ifTrue: [^method := smalltalk compileMethod: source].
  ast := foreign parse: self body.
  cm := smalltalk compileMethod: self template.
  method := ForeignMethod from: cm.
  method
    sourceCode: source;
    foreignCode: ast format;
    foreignParser: foreign
```

There are several things to explain here:

- The `#body` method, with the help of the `smalltalk` parser, answers with the foreign _body_, i.e., the part of the `source` code that comes after the pragma.
- The `#template` method answers with the source code of the Smalltalk method that will actually be executed when the hybrid method is invoked.
- The `ForeignMethod` class is a subclass of `CompiledMethod` that adds support to certain messages required from hybrid methods.
- The `#format` message sent to the `ast` is optional. I would recommend including it because it is nice to have your foreign code formatted as soon as you save your method.

In the _unary_ case we are now, the `#template` method has the following source code
```ruby
template
  ^self selector , '
  #code.
  #parser.
  ^#code'
```
where `#selector` answers, with the help of the `smalltalk` compiler, the method's selector; the following two symbols are placeholders for two slots in the literal frame that we will change below. Note that the compiled method will answer with the contents of the first literal slot.

```
ForeignMethod >> foreignCode: aString
  self literalAt: 1 put: aString
```
```
ForeignMethod >> foreignParser: aParser
  self literalAt: 2 put: aParser
```

The second literal holds the foreign parser, which may be needed for further processing (e.g., painting the source code).

Task 4: Macro Expansion
--

We will now address the case where the method has arguments which are inserted in the foreign code using `#` as the marker prefix. Let's refer to these prefixed identifiers as _hashed tokens_.

What we need is to dynamically replace all hashed tokens with the corresponding arguments. For doing this we will introduce a new class named `ParametricString`, which will implement this transformation as a service.

Basically this new object takes `aString` including _hashed tokens_ and a sequence of all possible tokens (in our case, the sequence of method arguments). Using this information the object produces a sequence of strings and indexes. The strings are the fragments between _hashed tokens_, the indexes refer to the sequence of _hashed tokens_. For instance if the inputs are:

- `'hello #world, this is a #test'.`
- `#('test' 'dummy' 'word')`

the object should generate the following sequence:

```
#('hello ' 3 ', this is a ' 1 '.')
```
with the somewhat clearer Squeak-braces syntax, this would be
```
{'hello'. 3. ', this is a '. 1. '.'}
```

Later on, when the object is required to _expand_ the tokens using actual arguments it will replace the indexes with the corresponding values, concatenating them all. The message to do this will be
```
aParametricString expandUsing: aCollection`.
```

Task 5: Hybrid method with arguments
--

Since we have already worked on the _unary_ case in **Task 3**, we only need to redo the `#template` method for the case where there are arguments.

The first change to consider is that what before was simply the `selector`, it is now a keyword message with the formal arguments. This can be simply accomplished with the help of the `smalltalk` compiler, so I will not go in detail here. We will just assume that `self selector` will answer with the signature of the method (i.e., `'keyword1: arg1 keyword2: arg2'`...).

Since the source code will be now a bit more complex, I will use the `#streamContents:` constructor.

```
template
  | arguments processor |
  arguments := self arguments.
  processor := ParametricString from: self body tokens: arguments.
  ^String streamContents: [:strm |
    strm
      nextPutAll: self selector;
      crtab;
      nextPutAll: '| ast |';
      crtab;
      nextPutAll: '#code.';
      crtab;
      nextPutAll: '#parser.';
      crtab;
      nextPutAll: 'ast := #parser parse: (#code expandUsing: {'.
    arguments
      do: [:arg | strm nextPutAll: arg]
      separatedBy: [strm nextPutAll: '. '].
    strm
      nextPutAll: '}).';
      crtab;
      nextPutAll: '^ast format']
```
