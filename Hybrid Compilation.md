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
2. Introduce a new subclass of `LiteralNode` named `TaggedNode`.
3. Consider the introduction of foreign parsers such as a `JsonParser`.
4. Introduce a new class of AST node named `ForeignNode`.
5. Process the body of the foreign script, according to its semantics.

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

One difference with tagged nodes is that here the _entire method_ is written in a foreign language. Why this variation is interesting? Because it will allow us to pass Smalltalk arguments to foreign method. Like this:
```smalltalk
jsonCoordinates: lat longitue: long
  <json>
  ^{
      "latitude": #lat,
      "longitude": #long
    }
```
