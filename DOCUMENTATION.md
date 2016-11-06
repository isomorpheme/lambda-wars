# Changes

We've mostly kept the structure of the starting framework, but we made a few changes:

* We split the `Controller` and `Model` modules into several submodules, one for
  each type.
* We also deleted the `View.hs` file, and instead added a module and typeclass
  called `Draw`, which any drawable type (`Player`, `World`, etc.) is an
  instance of.
* Some `Draw` instances got quite long and had a few helper functions, so we put   
  those in separate submodules of `View`.
* We removed `Controller.Time` and `Controller.Event`, and moved the functions
  in those modules to `Controller`.
* We modified `Model.World.initial` to take a `StdGen` instead of just a seed,
  so that we could keep the current random generator when resetting the world.

# Features

## Player Movement

Unlike in the example executables, the player does not have a constant speed in
our implementation. Instead, the player can thrust their spaceship forward,
which accelerates it in the direction it is pointing. Since the game is supposed
to be in space, the player's spaceship does not decelerate automatically.
Instead, the player will have to thrust in the opposite direction of the ship's
velocity.

<!-- TODO: wraparound -->

## Enemy Spawning and Movement

We implemented two enemy types: Seekers and Asteroids. Both types spawn
randomly, but never too close to the player, to keep the game fair. Seekers are
worth 5 points, whereas asteroids are only worth 1 point. Both types of enemy
spawn an explosion when they are destroyed, and they both kill the player when
they collide with the spaceship.

Seekers accelerate towards the spaceship -- they do not simply move towards it.
They also account for wraparound, always taking the shortest route to the
player. Since they keep accelerating, Seekers' velocity can become quite high.
This gives the player a chance to avoid them, since a Seeker can not change
their direction quick enough if the player moves.

Asteroids are given a random size, velocity and rotation when spawned. They do
not move towards the player, but can be quite large, and therefore hard to
avoid. When the player shoots an asteroid, it is split in two smaller halves,
which travel outward from the position of the original asteroid. Once an
asteroid is small enough, it will simply disappear when shot.

## Score Keeping

We implemented score keeping according to the requirements. When the player
dies however, the multiplier is reset, but not the score.

## Particle Effects

<!-- TODO -->

## Background

<!-- TODO -->
