# Tiki Tech

Tiki Tech is a football simulation engine/game that attempts to simulate the game of football from first principles. That is to say that this project begins with a representation of the physics of the motion of players and the ball and hope that, when coupled with an understanding of how to play the game and any relevant tactics, something that resembles the game of football will emerge.

The idea of this project is to be a testbed for implemeting a wide variety of football tactics and experimenting with how they might interact on the pitch.  Here I some things I'd like to capture:

- Fundamental model
  - Ball motion
    - Acceleration
    - Friction
    - Aerodynamics
    - Spin
  - Player motion
    - Acceleration
    - Running
    - Kicking
    - Jumping
    - Diving
    - Heading
- Custom tactics
  - Directness
    - Short passing
    - Long ball
  - Pressing
    - Pressing Traps
    - Counter-pressing
    - High/Mid/Low Blocks
  - Marking
    - Various zonal marking schemes
    - Man-marking
  - Formations
    - Positional rotations
    - Depedent upon phase of play
- Player preferences
- Player attributes

## Implementation

Tiki Tech uses a number of concepts to capture elements of the game.

- **Behaviours** (Football.Behaviours.*)
   
  These are actions that the players can perform such as and contain implementation describing how such behaviours are performed.

  - Dribbling
  - Kicking
  - Marking
  - Moving into spaces
  - Passing
  - Shooting

- **Intentions** (Football.Intentions.*)

  Determines how the players decide what their objectives are. Players will *intend* to perform one of the above behaviours. The implementation in these modules determines what behaviours they will decide to perform based on the phase of play and state of the game.

- **Understanding** (Football.Understanding.*)

  Determines how the players understand their game.  It provides information such as: 
  
  - The value of being in particular locations (i.e. expected goals)
  - The estimated chance of completing a pass
  - The distance to opposition players
  - The level of pressure a player is under (i.e. small voronoi polygons)
  - Tactical instructions such as a particular formation and role

- **Match Engine** (Football.Match.Engine)

  An implementation of the `Match` typeclass, it iteratively updates the game for each timestep, moving the ball and players according to their equations of motion and causes the players to decide upon and enact their next actions.

  The match engine makes heavy use of STM to handle atomic updates of the match state in a multithreaded execution context.


  


