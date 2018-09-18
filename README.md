2D Exploration Game
=================================================

This is a simple 2d exploration game written in OCaml.

It is intended to be a learning project and in time will hopefully serve as an example of how to write simple [terminal-based] games in OCaml.

## Build and Launch

1. Build `dune build @install`
2. Launch `2d-exploration-game`

## How to Play

* Use vim-style movement keys _h_, _j_, _k_, _l_ (left, down, up, right) to move
* Esc, Ctrl-C or 'q' to quit

## Features

### Current

- [x] Terminal graphics
- [x] Single-player
- [x] Movement using vim-style direction keys (H,J,K,L)
- [x] World generation with procedural psuedo-randomness
- [x] Entities - Humans and Trees
- [x] Items - Rocks
- [x] Players inventories
- [x] Centered game window
- [x] Worlds are not persistent and can't be saved

### Ideas for Future

- [ ] Multiplayer
- [ ] World item interaction
- [ ] Inventory item interaction
- [ ] World entity interaction
- [ ] Persistent worlds / save game
- [ ] World size > 128x128
- [ ] AI for world entities
- [ ] User interface
- [ ] Many more entities and items
- [ ] Wearable and weildable items
- [ ] Key binding configuration
- [ ] Terrain
- [ ] Turns

## OCaml Program Notes

### Structure and Code

For those interested in how this program is written/structured:

* Uses _notty_ for terminal graphics
* Uses _core.command_ for CLI parsing
* World is stored in a _list_ with items referenced via _association_
* Everything is immutable
* Modules are used and inherited
* _result_ and _option_ types are used heavily to avoid _Exceptions_ where possible
* No use of imperative language features

### Program Improvement Ideas

- [ ] Refactor randomness which is not really _random_
- [ ] Use a better suited container (other than _list_) for World
- [ ] Move modules to their own source files
- [ ] Define and use module interfaces
- [ ] Refactor rendering logic which feels a bit clunky
- [ ] Logging with levels and UI/file output

If you have any work or suggestions that improve the way this program is written, feel free to contact me or make a PR.
