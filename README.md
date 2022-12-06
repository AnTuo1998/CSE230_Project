# Build

```
stack build
stack exec BB
```

Emoji is supported for tmux and xterm based terminals (e.g. iTerm, mac Terminal...).

# Test

```
stack test
```

# *22Fall CSE230 Project*: Brick Breaker Game

**Group Member**: Hanlin Teng, Spencer Du, Wenzao Cui, Yunshu Zhou

## Milestone: Updates

### Game Logics

The game consists of the following key components:

- Game status: can either be ready to start, in progress or ended (win or lose).
- Game statistics: score, life count, time passed, etc.
- Balls: the balls on the court, which can be ordinary balls (bounce when hitting a brick) or fireballs (don't bounce when hitting a brick).
- Bricks: the living bricks in the game. They are initialized with a fixed hardness count; the bricks will disappear when hit by the `hardness` times by the balls.
- Buffs: the buffs in the game. Buff types include `FireBallBuff`, which turns all ordinary balls into fireballs, and `SplitBallBuff` which chooses a random ball in the court and splits it into two.

To update the game with each time tick, the following operations are performed:

- Update game status: check if the game is
  - won, if all bricks are smashed; or
  - lost, if the player's life count is zero or time is up.
- Handle hitting:
  - check if any bricks are hit, and if so,
    - update scores;
    - clear the bricks that have been hit the given `hardness` times, and trigger possible generations of buffs;
  - change direction if the balls hit the walls.
- Update moving objects: update the position of balls and buffs; clear the balls if out of bounds.

### UI Module

Our implementation contains multiple pages that utilize different Brick widgets and rendering routines for various functionalities. Pages can exchange states, and proceed depending on the final state of the previous page.

- Home Page: Home page consists of multiple entries to other pages. Its main component is a self-defined cursor that allows users to scroll up and down to enter different pages.
- Ranking Page: Ranking page reads username-score pairs from a txt file, sorts them by scores, and displays the top ten records.
- Start Page: Start page is where users can enter their usernames, and choose which level they want to play. We used the `Form` attribute in Brick to facilitate user inputs and updates. We also enforced multiple validations on user inputs, e.g., level can only be chosen from 0-5, and the username cannot be empty. Input boxes have different colors corresponding to valid or invalid inputs.
- Help Page: Help page shows a text of instructions.

### Challenges

- Restore the previous state when returned from another page: we chain our pages together, and states are passed through return values.  
- Handle invalid events (e.g., check invalid user inputs): we use the `Form` widget in Brick. It has built-in methods for input validation and display.

### Expectation

We think we'll be able to finish our project on time.

## Milestone: Proposal

### Project Description

We plan to build a brick breaker game where players can smash overhead walls with a perpetually bouncing ball. Players are able to move a board to catch and deflect the ball upwards. The game ends when the ball hits the ground (the player fails to bounce back the ball with the board) or the time limit is reached. The players should attempt to destroy wall pieces as many as possible to achieve highest scores in the time limit. We plan to add some extra features based on the traditional brick break game. Based on our Google search, there’s no existing implementation of the game with the `Brick` library.

<!-- ![Screenshot](pictures/brick-breaker.jpg) -->

<center><img width=60% src="pictures/brick-breaker.jpg"></img></center>

### Goal/Features

We plan to first build a basic version of the game that follows simplified rules, and then add different features incrementally from there. The functionalities we aim to achieve are outlined below :

#### Basic game logic

This includes generating the brick map, bouncing the ball in different directions, boundary detections, and keeping track of the status of each brick, etc.

#### User interface

We want to build a user-friendly homepage and other game pages that allow players to Enter, Quit, Restart, or Pause the game.

#### Features

- **Chances**: Players have multiple chances during one game. Every time they fail to catch the ball, a chance will be used, until there’s no chance available.

- **Time limit**: The game is timed so we need to implement a countdown timer.

- **Buff, debuff**: The game should be able to randomly generate buffs or debuffs that have a positive or negative impact on the game, e.g., the ability to move the board faster.

- **Scores**: The game should compute and keep track of the scores for each round (basic) and display the ranking (i.e. highest scores achieved) when it ends (stretch goal).

- **Levels**: We want to design multiple game levels and more diversified layouts with easy, medium, and hard difficulties. (stretch goal)

### Timeline

- 11.9 proposal submission
- 11.19 Milestone1: basic functionality
- 12.3 Milestone2: add-ons
- 12.9 final presentation

### References

- [Brick](https://github.com/jtdaugherty/brick)
- [Brick Breaker Game ScreenShot](https://store.steampowered.com/app/874780/Brick_Breaker_Premium/)
