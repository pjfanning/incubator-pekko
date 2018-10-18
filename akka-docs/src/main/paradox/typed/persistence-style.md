# Persistence - coding style

## Event handlers in the state

The section about @ref:[Changing Behavior](persistence.md#changing-behavior) described how commands and events
can be handled differently depending on the state. One can take that one step further and define the event
handler inside the state classes. In @ref:[next section the command handlers](#command-handlers-in-the-state) are
also defined in the state.

The state can be seen as your domain object and it should contain the core business logic. Then it's a matter
of taste if event handlers and command handlers should be defined in the state or be kept outside it.

Here we are using a bank account as the example domain. It has 3 state classes that are representing the lifecycle
of the account; `EmptyAccount`, `OpenedAccount`, and `ClosedAccount`.

Scala
:  @@snip [AccountExampleWithEventHandlersInState.scala](/akka-persistence-typed/src/test/scala/docs/akka/persistence/typed/AccountExampleWithEventHandlersInState.scala) { #account-entity }

TODO include corresponding example in Java

Notice how the `eventHandler` delegates to the `applyEvent` in the `Account` (state), which is implemented
in the concrete `EmptyAccount`, `OpenedAccount`, and `ClosedAccount`.

## Command handlers in the state

We can take the previous bank account example one step further by handling the commands in the state too.

Scala
:  @@snip [AccountExampleWithCommandHandlersInState.scala](/akka-persistence-typed/src/test/scala/docs/akka/persistence/typed/AccountExampleWithCommandHandlersInState.scala) { #account-entity }

TODO include corresponding example in Java

Notice how the command handler is delegating to `applyCommand` in the `Account` (state), which is implemented
in the concrete `EmptyAccount`, `OpenedAccount`, and `ClosedAccount`.

## Optional initial state

Sometimes it's not desirable to use a separate state class for the empty initial state, but rather treat that as
there is no state yet.
@java[`null` can then be used as the `emptyState`, but be aware of that the `state` parameter
will then be `null` for the first commands and events until the first event has be persisted to create the
non-null state. It's possible to use `Optional` instead of `null` but that results in rather much boilerplate
to unwrap the `Optional` state parameter and therefore `null` is probably preferred. The following example
illustrates using `null` as the `emptyState`.]
@scala[`Option[State]` can be used as the state type and `None` as the `emptyState`. Pattern matching
is then used in command and event handlers at the outer layer before delegating to the state or other methods.]

Scala
:  @@snip [AccountExampleWithOptionState.scala](/akka-persistence-typed/src/test/scala/docs/akka/persistence/typed/AccountExampleWithOptionState.scala) { #account-entity }

TODO include corresponding example in Java