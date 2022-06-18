# tic-tac-toe

Cardano smart contract for playing Tic-Tac-Toe

# Implemntation

```
          1   2   3
        .-----------.
     1  | X | O | X |
        *-----------*
     2  | O | X | O |
        *-----------*
     3  | O | X | O |
        *-----------*

```

## Features of the smart contract

- The entire game is implemented on-chain and has the following features

  - The on-chain code is capable of determining the winner of the game
  - It can identify if the game is a tie
  - The correct game is identified by embedding an NFT in all the script TXs
  - It can check if a new player (opponent) has joined the game.
  - It will check the game stake every time you interact with the script
  - It can check if a player is trying to cheat in the following scenarios
    - Make a double move
    - Try to overwrite the existing move
    - Try to steal the game stake
  - Can claim the reward before and after the deadline
  - Can claim your stake if the game is a tie
  - Can claim back your stake after the deadline if the game is left unfinished.

## Walkthrough of the game

- Player1 always initiates the game and is given the marker 'X'

  - The state of the game is held in the Datum and is updated carefully
  - When the game starts, the player will initialize the game with 555_555_555
  - A check for the initial conditions is set inside the contract and will flag an error if the initial conditions are not met.
  - Player1 also creates an NFT and sends it to the script initially. The NFT is used for identifying the correct game.

- Player2 can join the game if the game is waiting for a new player.

  - After the player2 has joined the game, the script will allow you to update the datum information for player2
  - Player2 will be given marker 'O'

- When a player wins the game, he can claim the reward immediately without waiting for the deadline to expire.

  - The same thing applies when the game is a tie.

- When the game is unfinished, you need to wait until the deadline has expired to get back your stake.

## Tests

- **testPlay** This will let you run a successful game and claim the reward.
- **testTie** This will let you run a game that was a tie and then you claim your stake back.

## Known issues

- Currently, the game uses **getContinuingOutputs** always, and the script expects a payment every time. Hence the NFT is stuck in the script at the end.
  - This function is currently being used for checking the datum.
  - A workaround can be created by creating a parameterized smart contract and using those parameters in the smart contract.
