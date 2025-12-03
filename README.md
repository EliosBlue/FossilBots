# Fryxbot Submission Skeleton

A submission for the [Fryxbot
project](https://github.com/rcdickerson/fryxbots).

This repository includes the definition and implementation of a programming language called BotLang. BotLang has
constructs for putting bots into different modes, turning bots right
and left, moving them forward, droppping and destroying beacons, and picking up and dropping fossils.

You can run the simulator by saying:

``` shell
stack run
```

The application that runs when you do this is defined in `app/Main.hs`.

The `src` directory contains the code which define and implement
ExampleLang:

  + `Controller.hs`: a Fryxbot controller which controls a robot by
    executing an BotLang program.

  + `Interpreter.hs`: an BotLang interpreter whose semantics are
    defined in terms of robot state changes.

  + `Language.hs`: the BotLang AST definition.

  + `Parser.hs`: an BotLang parser implemented using Megaparsec.


The `worlds` directory contains some example fields of play.

The `programs` directory contains an BoxLang program which moves the bot in a semi-box pattern then has the bot celebrate.
This is what is
currently loaded for the blue team by `app/Main.hs` when the
application is executed. (The gold team uses the RandomController from
the Fryxbot library.)

Good luck!
