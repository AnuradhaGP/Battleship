/* Battleship
* Author: anugp
* Creation date: 12/9/2024
*/
MACHINE
    Battleship

SETS
    PLAYER = { PLAYER_1 , PLAYER_2 } ;
    STATUS = { DEPLOY_STAGE , ONGOING_GAME , WINNER_PLAYER_1 , WINNER_PLAYER_2 } ; // status of the game
    MESSAGE = { SUCCESS , INVALID_PLACEMENT , SHIPS_ARE_ALREADY_PLACED , HIT , MISS , YOU_TRIED_THIS_TARGET_BEFORE } // messages to display

CONCRETE_CONSTANTS
    GRID , // to store the grid of the battlefield 
    gridSize , // stores the grid size
    shipCount // stores the number of ships per side
PROPERTIES
    gridSize = 10 &
    GRID = ( 1 .. gridSize ) * ( 1 .. gridSize ) &
    shipCount = 3
ABSTRACT_VARIABLES
    playerGrids , // stores the Player with their own grid
    fleet , // stores the Players ship placements on the grid with corresponding player.
    shots , // stores every shots that each player make
    turn , // keep tracking the current player
    gameState , // keep tracking game state
    hits , // how many of the shots have been successful
    opponent // keep tracking the opponent player at the time.
INVARIANT
    playerGrids : PLAYER --> POW ( GRID ) &
    fleet : PLAYER --> POW ( GRID ) &
    shots : PLAYER --> POW ( GRID ) &
    hits : PLAYER --> NAT &
    turn : PLAYER &
    opponent : PLAYER &
    turn /= opponent &
    gameState : STATUS &
    ! ( player ) . ( player : PLAYER => card ( fleet ( player ) ) <= shipCount )
INITIALISATION
    playerGrids := { PLAYER_1 |-> GRID , PLAYER_2 |-> GRID } ||
    fleet := { PLAYER_1 |-> {} , PLAYER_2 |-> {} } ||
    shots := { PLAYER_1 |-> {} , PLAYER_2 |-> {} } ||
    hits := { PLAYER_1 |-> 0 , PLAYER_2 |-> 0 } ||
    turn := PLAYER_1 ||
    opponent := PLAYER_2 ||
    gameState := DEPLOY_STAGE
OPERATIONS
    // deploy fleets
    report <-- deployFleet ( player , positions ) =
    PRE
        player : PLAYER &
        positions <: GRID &
        card ( positions ) = shipCount &
        gameState = DEPLOY_STAGE &
        fleet ( player ) = {}

    THEN

            fleet ( player ) := positions ||
            report := SUCCESS ||
            //if both players have placed their fleets then the game stage changing to ongoing game
            IF ( fleet ( PLAYER_1 ) /= {} or fleet ( PLAYER_2 ) /= {} ) THEN
                gameState := ONGOING_GAME
            END


    END ;

    //Shooting to opposition ships
    report <-- playerShoots ( target ) =
    PRE
        gameState = ONGOING_GAME &
        target : GRID

    THEN
        //if the target is a location that already shoot, then return error message
        IF ( shots ( turn ) /\ { target } = {} ) THEN
            shots ( turn ) := shots ( turn ) \/ { target } ||

            IF ( target : fleet ( opponent ) ) THEN
                report := HIT ||
                hits ( turn ) := succ ( hits ( turn ) ) ||

                //If 3 ships on other player's grid destroyed the current player is declared as the winner
                IF ( hits ( turn ) = pred ( shipCount ) ) THEN
                    IF ( turn = PLAYER_1 ) THEN
                        gameState := WINNER_PLAYER_1
                    ELSE
                        gameState := WINNER_PLAYER_2
                    END

                END
            ELSE
                report := MISS
            END ||

            turn := opponent ||
            opponent := turn
        ELSE
            report := YOU_TRIED_THIS_TARGET_BEFORE
        END

    END ;

    //locations of the remaining fleets
    shipsquares <-- shipLocations ( player ) =
    PRE
        player : PLAYER &
        gameState = ONGOING_GAME &
        fleet ( player ) /= {}
    THEN
        IF ( player = PLAYER_1 ) THEN

            shipsquares := fleet ( player ) - shots ( PLAYER_2 )

        ELSE

            shipsquares := fleet ( player ) - shots ( PLAYER_1 )

        END

    END ;

    // number of ships left on both players
    shipCounts <-- shipLeft =
    PRE
        gameState = ONGOING_GAME
    THEN
        shipCounts := { PLAYER_1 |-> card ( fleet ( PLAYER_1 ) - shots ( PLAYER_2 ) ) , PLAYER_2 |-> card ( fleet ( PLAYER_2 ) - shots ( PLAYER_1 ) ) }
    END ;

    //number of shots taken by a player
    shotCount <-- shotsTaken ( player ) =
    PRE
        player : PLAYER &
        gameState = ONGOING_GAME
    THEN
        shotCount := card ( shots ( player ) )
    END ;

    //Game Status

    report <-- gameStatus =
    PRE
        gameState : STATUS
    THEN
        report := gameState
    END

DEFINITIONS
    SET_PREF_RANDOMISE_ENUMERATION_ORDER == TRUE ;
    ANIMATION_FUNCTION ==
    ( { r , c , img | r : 1 .. ( gridSize + 1 ) & c : 1 .. ( ( 2 * gridSize ) + 1 ) & img = 0 }

        <+ { r , c , i | r : { 1 } & c : 1 .. ( ( 2 * gridSize ) + 1 ) & i = 6 } // filling top row with background tiles
        <+ { r , c , i | r : { 1 } & c : { 11 } & gameState = DEPLOY_STAGE & i = 9 } // Deploy game indicator
        <+ { r , c , i | r : { 1 } & c : { 11 } & gameState = ONGOING_GAME & i = 10 } // ongoing game indicator
        <+ { r , c , i | r : { 1 } & c : { 11 } & gameState = WINNER_PLAYER_1 & i = 11 } // player 1 wins indicator
        <+ { r , c , i | r : { 1 } & c : { 11 } & gameState = WINNER_PLAYER_2 & i = 12 } // player 2 wins indicator
        <+ { r , c , i | r : { 1 } & c : { 6 } & i = 4 } // player 1 indicator
        <+ { r , c , i | r : { 1 } & c : { 16 } & i = 5 } //player 2 indicator
        <+ { r , c , i | r : { 1 } & c : { 6 } & turn = PLAYER_1 & i = 7 } // player 1's turn indicator
        <+ { r , c , i | r : { 1 } & c : { 16 } & turn = PLAYER_2 & i = 8 } //player 2's turn indicator
        <+ { r , c , i | r : 2 .. ( gridSize + 1 ) & c : { 11 } & i = 6 } // grid separator
        <+ { r , c , i | ( r - 1 |-> c ) : fleet ( PLAYER_1 ) & i = 1 } // player 1 fleet
        <+ { r , c , i | ( r - 1 |-> c - 11 ) : fleet ( PLAYER_2 ) & i = 1 } //player 2 fleet
        <+ { r , c , i | ( r - 1 |-> c - 11 ) : shots ( PLAYER_1 ) & ( r - 1 |-> c - 11 ) /: fleet ( PLAYER_2 ) & i = 3 } // player 1 missed shots
        <+ { r , c , i | ( r - 1 |-> c ) : shots ( PLAYER_2 ) & ( r - 1 |-> c ) /: fleet ( PLAYER_1 ) & i = 3 } // player 2 missed shots
        <+ { r , c , i | ( r - 1 |-> c - 11 ) : shots ( PLAYER_1 ) & ( r - 1 |-> c - 11 ) : fleet ( PLAYER_2 ) & i = 2 } // player 1 hit shots
        <+ { r , c , i | ( r - 1 |-> c ) : shots ( PLAYER_2 ) & ( r - 1 |-> c ) : fleet ( PLAYER_1 ) & i = 2 } // player 2 hit shots
    ) ;

    ANIMATION_IMG0 == "images/cell.gif" ;
    ANIMATION_IMG1 == "images/ship.gif" ;
    ANIMATION_IMG2 == "images/sink.gif" ;
    ANIMATION_IMG3 == "images/miss.gif" ;
    ANIMATION_IMG4 == "images/P1.gif" ;
    ANIMATION_IMG5 == "images/P2.gif" ;
    ANIMATION_IMG6 == "images/back.gif" ;
    ANIMATION_IMG7 == "images/P1turn.gif" ;
    ANIMATION_IMG8 == "images/P2turn.gif" ;
    ANIMATION_IMG9 == "images/dep.gif" ;
    ANIMATION_IMG10 == "images/game.gif" ;
    ANIMATION_IMG11 == "images/p1win.gif" ;
    ANIMATION_IMG12 == "images/p2win.gif" ;

END
