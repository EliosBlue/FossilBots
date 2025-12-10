# BotLang Quick Reference

## Syntax Overview

### Basic Commands
```
moveForward         // Move one cell ahead
turnLeft            // Rotate 60° counter-clockwise
turnRight           // Rotate 60° clockwise
pickUpFossil        // Collect fossil at current location
dropFossil          // Leave fossil at current location
dropBeacon kind1    // Mark location (kind1-kind6 available)
destroyBeacon       // Remove beacon at current location
noop                // Do nothing (pass turn)
```

### Mode Definition
```
mode modeName {
  // commands here
}
```

### Sequencing
```
command1 ; command2 ; command3
```
Execute commands in order.

### Loops
```
for N {
  // body executes N times
}
```

### Conditionals

#### Check for Fossil
```
ifFossil
  trueAction
  falseAction
```
If fossil exists at current location, execute `trueAction`, else `falseAction`.

#### Check for Base
```
ifBase
  trueAction
  falseAction
```
If at base location, execute `trueAction`, else `falseAction`.

### Mode Calls
```
modeNameHere
```
Invoke another mode as a subroutine.

---

## Mode Categories in simplebot.exl

### Navigation (4 modes)
- `step` - Single step forward
- `turnCW` - Turn left
- `turnCCW` - Turn right
- `turnAround` - 180° turn

### Wandering (3 modes)
- `spiralSearch` - Expanding spiral pattern
- `zigzag` - Zigzag exploration
- `randomWalk1`, `randomWalk2`, `randomWalk3` - Various patterns

### Beacons (2 modes)
- `markFossilFound` - Mark with kind1 beacon
- `markExplored` - Mark with kind2 beacon

### Fossil Handling (2 modes)
- `collectFossil` - Pick up and mark
- `checkAndCollect` - Check then collect or move

### Base Navigation (3 modes)
- `moveTowardBase` - Head toward base (uses ifBase)
- `returnToBase` - Return with fossil
- `depositFossil` - Drop fossil at base

### Workers (5 modes)
- `fossilWorker` - Main single-bot behavior
- `fossilWorkerAlt` - Alternative search patterns
- `collector` - Collector role
- `scout` - Scout role
- `guard` - Guard role

---

## Program Flow

```
START
  |
  v
SEARCH (wander using search pattern)
  |
  v
FIND FOSSIL? --NO--> Back to SEARCH
  |
  YES
  |
  v
COLLECT (pickup + mark beacon)
  |
  v
RETURN TO BASE (navigate home)
  |
  v
AT BASE? --NO--> Keep moving toward base
  |
  YES
  |
  v
DEPOSIT (drop fossil)
  |
  v
EXPLORE (mark area as explored)
  |
  v
Back to SEARCH
```

---

## Beacon Kinds Reference

```
kind1   // Fossil discovery marker
kind2   // Explored area marker
kind3   // Available for custom use
kind4   // Available for custom use
kind5   // Available for custom use
kind6   // Available for custom use
```

---

## Example: Custom Search Mode

To add your own search pattern:

```
mode myCustomSearch {
  moveForward ;
  moveForward ;
  turnRight ;
  moveForward ;
  moveForward ;
  moveForward ;
  turnLeft
}
```

Then use it:
```
mode fossilWorkerCustom {
  for 1000000 {
    ifFossil collectFossil myCustomSearch ;
    returnToBase ;
    myCustomSearch
  }
}
```

---

## Tips for Writing Bot Programs

1. **Organize hierarchically**: Simple commands → combinations → complex behaviors
2. **Name clearly**: Use descriptive mode names
3. **Comment thoroughly**: Explain what each section does
4. **Test patterns**: Try different search patterns to see what works best
5. **Use loops wisely**: `for 1000000` creates long-running behavior
6. **Chain conditionals**: `ifFossil ... ifBase ...` creates complex logic

---

## Common Patterns

### Find and Mark
```
ifFossil {
  pickUpFossil ;
  dropBeacon kind1
} moveForward
```

### Navigate Home
```
ifBase {
  dropFossil ;
  moveForward
} moveForward
```

### Repeated Search
```
for 100 {
  moveForward ;
  ifFossil pickUpFossil moveForward
}
```

### Complex Behavior
```
for 1000 {
  ifFossil {
    pickUpFossil ;
    dropBeacon kind1 ;
    ifBase dropFossil moveForward
  } moveForward ;
  spiralSearch
}
```

---

## Debugging Tips

1. **Test in isolation**: Create simple test modes first
2. **Verify conditionals**: Make sure `ifFossil` and `ifBase` work as expected
3. **Check loops**: Ensure loop counts are reasonable
4. **Monitor beacons**: Verify beacons are placed where expected
5. **Trace execution**: Add comments to follow execution flow

---

## Files in FossilBots Project

```
src/
  Language.hs       - AST definitions
  Parser.hs         - BotLang parser
  Interpreter.hs    - BotLang evaluator
  Controller.hs     - Main control logic
  TypeChecker.hs    - Type checking (if used)

programs/
  simplebot.exl     - The fossil wanderer program
  
worlds/
  example0.world    - Test environments
  example1.world
  example2.world
  example3.world

docs/
  PROGRAM_GUIDE.md             - Detailed program documentation
  LANGUAGE_ENHANCEMENTS.md     - Proposed language additions
  (this file)                  - Quick reference
```

---

## Next Steps

1. **Run the program** in your simulator with different world files
2. **Experiment**: Modify search patterns and observe results
3. **Optimize**: Adjust loop counts and search strategies for your worlds
4. **Extend**: Add new modes to customize bot behavior
5. **Propose**: Use LANGUAGE_ENHANCEMENTS.md to guide language improvements
