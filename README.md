Hamerkop
========

A console-based application that simulates Social Networks behaviours
---------------------------------------------------------------------

Table of Contents
-----------------

* [Features](#features)
* [Running the application](#running-the-application)
* [Structure](#structure)
* [Storage](#storage)
* [Scalability](#scalability)
* [IO / Environment](#io--environment)
* [Tests](#tests)
* [Further Improvements](#further-improvements)

## Features

You can post messages:

```txt
@bob -> It's a lovely weather today!

bob: It's a lovely weather today!

@alice -> perfect day for hiking

alice: perfect day for hiking
```

Read users' posts:

```txt
@bob

what did you guys think about the trailer for The Last Jedi? (2 seconds ago)
it's a lovely weather today! (5 minutes ago)
```

Follow users and see their feed:

```txt
@bob follows alice

bob is now following alice

@bob wall

bob: what did you guys think about the trailer for The Last Jedi? (2 seconds ago)
alice: perfect day for hiking (51 seconds ago)
bob: it's a lovely weather today! (1 minute ago)
```

## Running the application

To run the application, simply clone this repository and run the command `./hamerkop` from the project directory:

```
$ ./hamerkop

Welcome to Hamerkop, a Social Network simulation.
Type :help to see the list of available commands.

```


## Structure

Hamerkop was built with simplicity and scalability in mind, trying to merge the requirements with
possibility for future extension. The requirements met are:

1. Console-based
1. In-memory storage
1. Local (all users will use the same terminal)
1. No pre-defined list of users
1. Create users upon first post command
1. Four available commands: post, read, follow, wall

Along with the four required commands, a `help` command was provided, so users can see what commands are available and how they work.

The project was divided in three main modules: [Main][1], [Env][2], [Commands][3]

The **Main** Module is responsible for fetching the user input and return the result from the input evaluation.
It also contains the list of available commands. Future commands can be added by updating this list.

**Env** Module's main responsibility is to evaluate the user input and find a Command which can execute it.

The **Commands** Module holds the Action Runners, that is, the functions that will _apply_ the user commands, such as display a list of posts or store a new post/user.

## Storage

As the application is completely run in-memory, a [record][4] was created, named `Env` such as follows:

```haskell
data Env
  = Env
    { cmds :: Commands
    , users :: Users
    , eTime :: UTCTime }
```

This record holds the list of available Commands, the list of Users and the time of the latest user-entered command.

## Scalability

Although the lack of a Database Management System (DBMS) has a huge negative impact in terms of scalability, the project
focused on other aspects, such as the easiness of adding new commands, as well as the possibility of adding _System Commands_.

To implement the Commands structure, I took as inspiration the method applied in the [Stack project][5]. Therefore,
each Command in Hamerkop is a record of a _name_, _description_ and an _ActionRunner_. Using this structure, new commands can be added by simply updating the [Commands list][6] and implementing its specific _ActionRunner_, eliminating the need to change other functions.

Moreover, when the user enters a command, its input is parsed to an [Action][7]. The type of Action will depend on the user input:

1. if the input starts with `@`, such as `@bob -> hello`, it is parsed to `UserAct`
1. if the input starts with `:`, such as `:help`, it is parsed to `SysAct`

This structure was designed so that more commands can be added in the future, without syntax conflicts.
Examples of future commands are:

`<@user> unfollow <user>`

`<@user> remove <post>`

`:users` _displays the list of stored users_

`:remove <user>`

`:new <user>` _creates a new user without posts_

## IO / Environment

The running environment, represented by the data record `Env`, is evaluated and executed in terms of a [State Monad][8].
Each time the user enters a command, the application will:

1. Update the Environment's date/time;
1. Evaluate the Action in the current Enviroment;
1. Return the result of the evaluation along with the updated Environment;

## Tests

The tests were written in [HSpec][9] as part of the development process. Although tests were written for some edge cases,
such as [reading posts from an unexisting user][10], the majority of the tests considered only successful scenarios, as requested in the Code Challenge description. Still, common _unsuccessful_ scenarios were handled with proper messages.
For example:

```
:remove bob

Command remove not found.

@bob wall

Oops! bob's wall seems empty.
```

## Further Improvements

A list of further improvements can be drawn from the current stage of the project:

1. Introduce `unfollow` command;
1. Introduce `remove <post>` command;
1. Add Unique Identifiers to Users and Posts, so that future commands can operate on them;
1. With more commands added, split `Commands` module so that each module handles only one Command;
1. Use the [YAML package][11] to create test stubs;
1. Use [QuickCheck][12] to generate edge case scenarios for tests.

The introduction of a DBMS was left out of the list because persisting the user data is not part of the current goal of the project.

[1]: https://github.com/lpalma/hamerkop/blob/master/app/Main.hs
[2]: https://github.com/lpalma/hamerkop/blob/master/src/Env.hs
[3]: https://github.com/lpalma/hamerkop/blob/master/src/Commands.hs
[4]: https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_.28Record_Syntax.29
[5]: https://github.com/commercialhaskell/stack/blob/master/src/main/Main.hs#L245-L248
[6]: https://github.com/lpalma/hamerkop/blob/master/app/Main.hs#L41-L57
[7]: https://github.com/lpalma/hamerkop/blob/master/src/Types.hs#L50-L59
[8]: https://wiki.haskell.org/State_Monad
[9]: https://hspec.github.io/
[10]: https://github.com/lpalma/hamerkop/blob/master/test/CommandsSpec.hs#L35-L37
[11]: https://github.com/snoyberg/yaml
[12]: https://github.com/nick8325/quickcheck
