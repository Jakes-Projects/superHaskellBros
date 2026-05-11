# superHaskellBros

**Project Group 2**

**Team Members:**
- Jake Alsept
- Ayman Hassen

## About the Project

superHaskellBros is a Haskell-based platformer game inspired by classic side-scrolling Mario-style gameplay. The project includes multiple levels, enemies, power-ups, coins, music, sound effects, and different level themes such as overworld, underground, underwater, and castle stages.

## Features

- Playable Mario-style character movement
- Multiple themed levels
- Enemies such as Goombas, Koopas, Piranha Plants, Cheep-cheeps, Bloopers, and Bowser
- Power-ups including Mushroom and Fire Flower
- Fireball mechanics
- Coins and question blocks
- Moving platforms
- Flagpole level endings
- Music and sound effects
- Custom sprites and level design

## Controls

| Key | Action |
| --- | --- |
| Left Arrow | Move left |
| Right Arrow | Move right |
| Up / Jump Key | Jump or swim |
| Run Key | Run or shoot fireballs when Fire Mario |
| Down Arrow | Crouch when Big or Fire Mario |
``` Same controls using WASD

## How to Run

From the project directory, run the project using the Haskell build command your group has been using. For example:

```bash
stack run
```

or, if your project uses Cabal:

```bash
cabal run
```Type “killall afplay” in the terminal to make the music stop
```

```run “chmod +x run.sh” in the main SuperHaskellBros directory. Then, to start game, use “./run.sh” instead of “cabal run”, this should stop the music when you close the window.

## Project Structure

The project is organized into separate Haskell modules for readability and easier debugging:

- `Main.hs` - starts the game
- `Types.hs` - shared data types
- `Constants.hs` - game constants
- `Level.hs` - level layouts and enemy placement
- `Mario.hs` - player input and Mario behavior
- `Physics.hs` - collision and movement physics
- `Enemy.hs` - enemy behavior and enemy collisions
- `PowerUp.hs` - power-up behavior and block interactions
- `Fireball.hs` - fireball spawning and movement
- `Rendering.hs` - drawing sprites, tiles, enemies, and UI
- `Music.hs` - background music handling
- `Sound.hs` - sound effect handling