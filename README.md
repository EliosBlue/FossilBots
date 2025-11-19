# Fryxbot Submission Skeleton

The skeleton of a submission for the [Fryxbot
project](https://github.com/rcdickerson/fryxbots).

This repository includes the definition and implementation of a
bare-bones programming language called ExampleLang. ExampleLang has
constructs for putting bots into different modes, turning bots right
and left, and looping a set of instructions forever. (You should
define and implement a more useful language!)

You can run the simulator by saying:

``` shell
stack run
```

The application that runs when you do this is defined in `app/Main.hs`.

The `src` directory contains the code which define and implement
ExampleLang:

  + `Controller.hs`: a Fryxbot controller which controls a robot by
    executing an ExampleLang program.

  + `Interpreter.hs`: an ExampleLang interpreter whose semantics are
    defined in terms of robot state changes.

  + `Language.hs`: the ExampleLang AST definition.

  + `Parser.hs`: an ExampleLang parser implemented using Megaparsec.


The `worlds` directory contains some example fields of play.

The `programs` directory contains an ExampleLang program which spins
robots clockwise or counter-clockwise forever. This is what is
currently loaded for the blue team by `app/Main.hs` when the
application is executed. (The gold team uses the RandomController from
the Fryxbot library.) Again, you should devise a more useful strategy!

Good luck!
