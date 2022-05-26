# tic-tac-toe

Cardano smart contract for playing Tic-Tac-Toe

# Implemntation

```
        .-----------.
        | X | O | X |
        *-----------*
        | O | X | O |
        *-----------*
        | O | X | O |
        *-----------*

```

- Create a parameterized contact with the following fields

  - ( game choice, pub keys of the player)
    - the game choice of the player is hardcoded at the begin of the game
  - game stake
  - deadline for the game [The game will be valid with the interval]
  - use a NFT to identify the game

- A game is started by one of the player and will be waiting for the other player.
  - For a given game the parameters for the contract will be fixed. i.e each game will have a unique script address.
- When a game is started by a player the State of the game will be initiated as follows
  - `[(("p11","1"),("p11","1")),(("p12","1"),("p12","1")),(("p13","1"),("p13","1"))]`
- The state of the game is stored in the Datum. The Datum will be carefully updated
- Players will be able to interact with the game using the Redeemer
  - The redeemer will contain the following fields
    - Game choice (either 'X' or 'O')
    - Players can claim the reward before the deadline if one of them won the game
    - If the game outcome is **not determined**/**or a tie** before the deadline the funds will be refunded.
