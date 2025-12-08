# FossilBot Wanderer Program Guide

## Overview

The new `simplebot.exl` program implements intelligent fossil collection behavior where bots wander the world, discover fossils, mark locations with beacons, and return fossils to their base.

## Program Architecture

### 1. Navigation Modes (Basic Movement)
- **`step`** - Move forward one space
- **`turnCW`** - Turn left (counterclockwise)
- **`turnCCW`** - Turn right (clockwise)  
- **`turnAround`** - Do a 180-degree turn

### 2. Wandering Modes (Search Patterns)
These modes implement different exploration strategies:

- **`spiralSearch`** - Explores in an expanding spiral pattern, good for systematic coverage
- **`zigzag`** - Creates a zigzag walking pattern to cover terrain methodically
- **`randomWalk1`, `randomWalk2`, `randomWalk3`** - Various random-like patterns for diverse exploration

### 3. Beacon Marking
- **`markFossilFound`** - Marks a fossil discovery with a `kind1` beacon
- **`markExplored`** - Marks explored areas with a `kind2` beacon

### 4. Fossil Collection
- **`collectFossil`** - Picks up a fossil and immediately marks the location
- **`checkAndCollect`** - Checks if a fossil is present; if yes, collects it; if no, continues wandering

### 5. Base Navigation
- **`moveTowardBase`** - Uses base detection (`ifBase`) to know when home
- **`returnToBase`** - Conditional check to drop fossil when at base, otherwise continues moving
- **`depositFossil`** - Drops fossil at base and moves away to search again

### 6. Main Worker Modes
- **`fossilWorker`** - Primary single-bot behavior: search → collect → return → deposit → repeat
- **`fossilWorkerAlt`** - Alternative worker using different search patterns
- **`collector`** - Specialized collector role for multi-bot systems
- **`scout`** - Scout role that explores different areas
- **`guard`** - Guard role that stays near base

## Usage

### Single Bot Mode (Default)
```plaintext
mode main {
  fossilWorker
}
```

### Multi-Bot Coordination (Uncomment in file)
```plaintext
mode main {
  collector ; scout ; guard
}
```

## How the Program Works

1. **Wandering**: Bot starts with a search pattern (spiral, zigzag, or random walk)
2. **Detection**: `ifFossil` conditional checks for fossils at current location
3. **Collection**: When fossil found → pick up → mark location with beacon
4. **Return**: `ifBase` conditional checks if bot is at base location
5. **Deposit**: When at base → drop fossil → move away
6. **Loop**: Repeat indefinitely (via large `for` loops)

## Control Flow Example

```
for 1000000 iterations {
  ifFossil {
    pickUpFossil
    dropBeacon kind1
  } else {
    moveForward (continue searching)
  }
  
  ifBase {
    dropFossil
    moveForward
  } else {
    randomWalk (navigate toward base)
  }
  
  spiralSearch (continue exploring)
}
```

## Beacon Strategy

- **Kind1**: Marks fossil discovery locations (useful for tracking high-value areas)
- **Kind2**: Marks explored areas (helps avoid redundant searching)
- Additional beacon kinds (Kind3-Kind6) available if needed

## Language Features Used

The program uses the following BotLang features:

1. **Mode Definitions**: `mode name { body }`
2. **Conditionals**: 
   - `ifFossil then_branch else_branch` - Checks if fossil at current location
   - `ifBase then_branch else_branch` - Checks if at base location
3. **Loops**: `for N { body }` - Repeats body N times
4. **Sequencing**: `term1 ; term2` - Execute in sequence
5. **Commands**:
   - `moveForward` - Move one space ahead
   - `turnLeft` / `turnRight` - Rotate 90 degrees
   - `pickUpFossil` - Collect fossil from current location
   - `dropFossil` - Leave fossil at current location
   - `dropBeacon kind` - Mark location with beacon

## Potential Language Enhancements

If you want to make the program even more sophisticated, consider adding these features:

### 1. Repeat-Until Loops
```
repeatUntil { condition } { body }
```
Executes body until condition is true. Useful for "return to base" logic:
```
repeatUntil { ifBase } { moveTowardBase }
```

### 2. Random Choice
```
choose { action1 } { action2 }
```
Randomly picks one of two actions at runtime. Useful for non-deterministic behavior.

### 3. Team Coordination
```
signal channel message
wait channel
```
Allow bots to communicate, useful for multi-bot coordination.

### 4. Sensory Conditions
```
ifFossilNearby distance
ifBeaconNear kind
ifTeammateNear
```
Better sensing for smarter decisions.

### 5. State Variables
```
set $counter 0
increment $counter
ifGreater $counter 5 { ... }
```
Allow bots to maintain internal state.

### 6. Function Definitions (Not just modes)
```
function gatherFossils(area_size) {
  for area_size { ... }
}
```
Currently modes don't take parameters; parameterized functions would help.

## Testing the Program

To test the program with different world configurations:

```bash
# Ensure simplebot.exl is in programs/ directory
# The world files (example0.world, example1.world, etc.) define the environment

# Run with the interpreter/simulator for your FossilBots system
```

## Notes

- The `for 1000000` loops are meant to run "indefinitely" (very long times) during simulation
- Bots may collide or interfere with each other in multi-bot mode; this is expected behavior
- Beacon kinds (1-6) can be customized for different purposes
- The program is designed to work with the existing BotLang syntax and semantics
