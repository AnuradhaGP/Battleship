Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Battleship))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Battleship))==(Machine(Battleship));
  Level(Machine(Battleship))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Battleship)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Battleship))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Battleship))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Battleship))==(?);
  List_Includes(Machine(Battleship))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Battleship))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Battleship))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Battleship))==(?);
  Context_List_Variables(Machine(Battleship))==(?);
  Abstract_List_Variables(Machine(Battleship))==(?);
  Local_List_Variables(Machine(Battleship))==(opponent,hits,gameState,turn,shots,fleet,playerGrids);
  List_Variables(Machine(Battleship))==(opponent,hits,gameState,turn,shots,fleet,playerGrids);
  External_List_Variables(Machine(Battleship))==(opponent,hits,gameState,turn,shots,fleet,playerGrids)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Battleship))==(?);
  Abstract_List_VisibleVariables(Machine(Battleship))==(?);
  External_List_VisibleVariables(Machine(Battleship))==(?);
  Expanded_List_VisibleVariables(Machine(Battleship))==(?);
  List_VisibleVariables(Machine(Battleship))==(?);
  Internal_List_VisibleVariables(Machine(Battleship))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Battleship))==(btrue);
  Gluing_List_Invariant(Machine(Battleship))==(btrue);
  Expanded_List_Invariant(Machine(Battleship))==(btrue);
  Abstract_List_Invariant(Machine(Battleship))==(btrue);
  Context_List_Invariant(Machine(Battleship))==(btrue);
  List_Invariant(Machine(Battleship))==(playerGrids: PLAYER --> POW(GRID) & fleet: PLAYER --> POW(GRID) & shots: PLAYER --> POW(GRID) & hits: PLAYER --> NAT & turn: PLAYER & opponent: PLAYER & turn/=opponent & gameState: STATUS & !player.(player: PLAYER => card(fleet(player))<=shipCount))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Battleship))==(btrue);
  Abstract_List_Assertions(Machine(Battleship))==(btrue);
  Context_List_Assertions(Machine(Battleship))==(btrue);
  List_Assertions(Machine(Battleship))==(btrue)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(Battleship))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(Battleship))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Battleship))==(playerGrids,fleet,shots,hits,turn,opponent,gameState:={PLAYER_1|->GRID,PLAYER_2|->GRID},{PLAYER_1|->{},PLAYER_2|->{}},{PLAYER_1|->{},PLAYER_2|->{}},{PLAYER_1|->0,PLAYER_2|->0},PLAYER_1,PLAYER_2,DEPLOY_STAGE);
  Context_List_Initialisation(Machine(Battleship))==(skip);
  List_Initialisation(Machine(Battleship))==(playerGrids:={PLAYER_1|->GRID,PLAYER_2|->GRID} || fleet:={PLAYER_1|->{},PLAYER_2|->{}} || shots:={PLAYER_1|->{},PLAYER_2|->{}} || hits:={PLAYER_1|->0,PLAYER_2|->0} || turn:=PLAYER_1 || opponent:=PLAYER_2 || gameState:=DEPLOY_STAGE)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Battleship))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Battleship))==(btrue);
  List_Constraints(Machine(Battleship))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Battleship))==(deployFleet,playerShoots,shipLocations,shipLeft,shotsTaken,gameStatus);
  List_Operations(Machine(Battleship))==(deployFleet,playerShoots,shipLocations,shipLeft,shotsTaken,gameStatus)
END
&
THEORY ListInputX IS
  List_Input(Machine(Battleship),deployFleet)==(player,positions);
  List_Input(Machine(Battleship),playerShoots)==(target);
  List_Input(Machine(Battleship),shipLocations)==(player);
  List_Input(Machine(Battleship),shipLeft)==(?);
  List_Input(Machine(Battleship),shotsTaken)==(player);
  List_Input(Machine(Battleship),gameStatus)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Battleship),deployFleet)==(report);
  List_Output(Machine(Battleship),playerShoots)==(report);
  List_Output(Machine(Battleship),shipLocations)==(shipsquares);
  List_Output(Machine(Battleship),shipLeft)==(shipCounts);
  List_Output(Machine(Battleship),shotsTaken)==(shotCount);
  List_Output(Machine(Battleship),gameStatus)==(report)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Battleship),deployFleet)==(report <-- deployFleet(player,positions));
  List_Header(Machine(Battleship),playerShoots)==(report <-- playerShoots(target));
  List_Header(Machine(Battleship),shipLocations)==(shipsquares <-- shipLocations(player));
  List_Header(Machine(Battleship),shipLeft)==(shipCounts <-- shipLeft);
  List_Header(Machine(Battleship),shotsTaken)==(shotCount <-- shotsTaken(player));
  List_Header(Machine(Battleship),gameStatus)==(report <-- gameStatus)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Battleship),deployFleet)==(player: PLAYER & positions <: GRID & card(positions) = shipCount & gameState = DEPLOY_STAGE & fleet(player) = {});
  List_Precondition(Machine(Battleship),playerShoots)==(gameState = ONGOING_GAME & target: GRID);
  List_Precondition(Machine(Battleship),shipLocations)==(player: PLAYER & gameState = ONGOING_GAME & fleet(player)/={});
  List_Precondition(Machine(Battleship),shipLeft)==(gameState = ONGOING_GAME);
  List_Precondition(Machine(Battleship),shotsTaken)==(player: PLAYER & gameState = ONGOING_GAME);
  List_Precondition(Machine(Battleship),gameStatus)==(gameState: STATUS)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Battleship),gameStatus)==(gameState: STATUS | report:=gameState);
  Expanded_List_Substitution(Machine(Battleship),shotsTaken)==(player: PLAYER & gameState = ONGOING_GAME | shotCount:=card(shots(player)));
  Expanded_List_Substitution(Machine(Battleship),shipLeft)==(gameState = ONGOING_GAME | shipCounts:={PLAYER_1|->card(fleet(PLAYER_1)-shots(PLAYER_2)),PLAYER_2|->card(fleet(PLAYER_2)-shots(PLAYER_1))});
  Expanded_List_Substitution(Machine(Battleship),shipLocations)==(player: PLAYER & gameState = ONGOING_GAME & fleet(player)/={} | player = PLAYER_1 ==> shipsquares:=fleet(player)-shots(PLAYER_2) [] not(player = PLAYER_1) ==> shipsquares:=fleet(player)-shots(PLAYER_1));
  Expanded_List_Substitution(Machine(Battleship),playerShoots)==(gameState = ONGOING_GAME & target: GRID | shots(turn)/\{target} = {} ==> (shots:=shots<+{turn|->(shots(turn)\/{target})} || (target: fleet(opponent) ==> (report,hits:=HIT,hits<+{turn|->succ(hits(turn))} || (hits(turn) = pred(shipCount) ==> (turn = PLAYER_1 ==> gameState:=WINNER_PLAYER_1 [] not(turn = PLAYER_1) ==> gameState:=WINNER_PLAYER_2) [] not(hits(turn) = pred(shipCount)) ==> skip)) [] not(target: fleet(opponent)) ==> report:=MISS) || turn:=opponent || opponent:=turn) [] not(shots(turn)/\{target} = {}) ==> report:=YOU_TRIED_THIS_TARGET_BEFORE);
  Expanded_List_Substitution(Machine(Battleship),deployFleet)==(player: PLAYER & positions <: GRID & card(positions) = shipCount & gameState = DEPLOY_STAGE & fleet(player) = {} | fleet,report:=fleet<+{player|->positions},SUCCESS || (fleet(PLAYER_1)/={} or fleet(PLAYER_2)/={} ==> gameState:=ONGOING_GAME [] not(fleet(PLAYER_1)/={} or fleet(PLAYER_2)/={}) ==> skip));
  List_Substitution(Machine(Battleship),deployFleet)==(fleet(player):=positions || report:=SUCCESS || IF fleet(PLAYER_1)/={} or fleet(PLAYER_2)/={} THEN gameState:=ONGOING_GAME END);
  List_Substitution(Machine(Battleship),playerShoots)==(IF shots(turn)/\{target} = {} THEN shots(turn):=shots(turn)\/{target} || IF target: fleet(opponent) THEN report:=HIT || hits(turn):=succ(hits(turn)) || IF hits(turn) = pred(shipCount) THEN IF turn = PLAYER_1 THEN gameState:=WINNER_PLAYER_1 ELSE gameState:=WINNER_PLAYER_2 END END ELSE report:=MISS END || turn:=opponent || opponent:=turn ELSE report:=YOU_TRIED_THIS_TARGET_BEFORE END);
  List_Substitution(Machine(Battleship),shipLocations)==(IF player = PLAYER_1 THEN shipsquares:=fleet(player)-shots(PLAYER_2) ELSE shipsquares:=fleet(player)-shots(PLAYER_1) END);
  List_Substitution(Machine(Battleship),shipLeft)==(shipCounts:={PLAYER_1|->card(fleet(PLAYER_1)-shots(PLAYER_2)),PLAYER_2|->card(fleet(PLAYER_2)-shots(PLAYER_1))});
  List_Substitution(Machine(Battleship),shotsTaken)==(shotCount:=card(shots(player)));
  List_Substitution(Machine(Battleship),gameStatus)==(report:=gameState)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Battleship))==(GRID,gridSize,shipCount);
  Inherited_List_Constants(Machine(Battleship))==(?);
  List_Constants(Machine(Battleship))==(GRID,gridSize,shipCount)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Battleship),PLAYER)==({PLAYER_1,PLAYER_2});
  Context_List_Enumerated(Machine(Battleship))==(?);
  Context_List_Defered(Machine(Battleship))==(?);
  Context_List_Sets(Machine(Battleship))==(?);
  List_Valuable_Sets(Machine(Battleship))==(?);
  Inherited_List_Enumerated(Machine(Battleship))==(?);
  Inherited_List_Defered(Machine(Battleship))==(?);
  Inherited_List_Sets(Machine(Battleship))==(?);
  List_Enumerated(Machine(Battleship))==(PLAYER,STATUS,MESSAGE);
  List_Defered(Machine(Battleship))==(?);
  List_Sets(Machine(Battleship))==(PLAYER,STATUS,MESSAGE);
  Set_Definition(Machine(Battleship),STATUS)==({DEPLOY_STAGE,ONGOING_GAME,WINNER_PLAYER_1,WINNER_PLAYER_2});
  Set_Definition(Machine(Battleship),MESSAGE)==({SUCCESS,INVALID_PLACEMENT,SHIPS_ARE_ALREADY_PLACED,HIT,MISS,YOU_TRIED_THIS_TARGET_BEFORE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Battleship))==(?);
  Expanded_List_HiddenConstants(Machine(Battleship))==(?);
  List_HiddenConstants(Machine(Battleship))==(?);
  External_List_HiddenConstants(Machine(Battleship))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Battleship))==(btrue);
  Context_List_Properties(Machine(Battleship))==(btrue);
  Inherited_List_Properties(Machine(Battleship))==(btrue);
  List_Properties(Machine(Battleship))==(gridSize = 10 & GRID = (1..gridSize)*(1..gridSize) & shipCount = 3 & PLAYER: FIN(INTEGER) & not(PLAYER = {}) & STATUS: FIN(INTEGER) & not(STATUS = {}) & MESSAGE: FIN(INTEGER) & not(MESSAGE = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Battleship),deployFleet)==(?);
  List_ANY_Var(Machine(Battleship),playerShoots)==(?);
  List_ANY_Var(Machine(Battleship),shipLocations)==(?);
  List_ANY_Var(Machine(Battleship),shipLeft)==(?);
  List_ANY_Var(Machine(Battleship),shotsTaken)==(?);
  List_ANY_Var(Machine(Battleship),gameStatus)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Battleship)) == (GRID,gridSize,shipCount,PLAYER,STATUS,MESSAGE,PLAYER_1,PLAYER_2,DEPLOY_STAGE,ONGOING_GAME,WINNER_PLAYER_1,WINNER_PLAYER_2,SUCCESS,INVALID_PLACEMENT,SHIPS_ARE_ALREADY_PLACED,HIT,MISS,YOU_TRIED_THIS_TARGET_BEFORE | ? | opponent,hits,gameState,turn,shots,fleet,playerGrids | ? | deployFleet,playerShoots,shipLocations,shipLeft,shotsTaken,gameStatus | ? | ? | ? | Battleship);
  List_Of_HiddenCst_Ids(Machine(Battleship)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Battleship)) == (GRID,gridSize,shipCount);
  List_Of_VisibleVar_Ids(Machine(Battleship)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Battleship)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Battleship)) == (Type(PLAYER) == Cst(SetOf(etype(PLAYER,0,1)));Type(STATUS) == Cst(SetOf(etype(STATUS,0,3)));Type(MESSAGE) == Cst(SetOf(etype(MESSAGE,0,5))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Battleship)) == (Type(PLAYER_1) == Cst(etype(PLAYER,0,1));Type(PLAYER_2) == Cst(etype(PLAYER,0,1));Type(DEPLOY_STAGE) == Cst(etype(STATUS,0,3));Type(ONGOING_GAME) == Cst(etype(STATUS,0,3));Type(WINNER_PLAYER_1) == Cst(etype(STATUS,0,3));Type(WINNER_PLAYER_2) == Cst(etype(STATUS,0,3));Type(SUCCESS) == Cst(etype(MESSAGE,0,5));Type(INVALID_PLACEMENT) == Cst(etype(MESSAGE,0,5));Type(SHIPS_ARE_ALREADY_PLACED) == Cst(etype(MESSAGE,0,5));Type(HIT) == Cst(etype(MESSAGE,0,5));Type(MISS) == Cst(etype(MESSAGE,0,5));Type(YOU_TRIED_THIS_TARGET_BEFORE) == Cst(etype(MESSAGE,0,5));Type(GRID) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(gridSize) == Cst(btype(INTEGER,?,?));Type(shipCount) == Cst(btype(INTEGER,?,?)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Battleship)) == (Type(opponent) == Mvl(etype(PLAYER,?,?));Type(hits) == Mvl(SetOf(etype(PLAYER,0,1)*btype(INTEGER,0,MAXINT)));Type(gameState) == Mvl(etype(STATUS,?,?));Type(turn) == Mvl(etype(PLAYER,?,?));Type(shots) == Mvl(SetOf(etype(PLAYER,0,1)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(fleet) == Mvl(SetOf(etype(PLAYER,0,1)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(playerGrids) == Mvl(SetOf(etype(PLAYER,0,1)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Battleship)) == (Type(gameStatus) == Cst(etype(STATUS,?,?),No_type);Type(shotsTaken) == Cst(btype(INTEGER,?,?),etype(PLAYER,?,?));Type(shipLeft) == Cst(SetOf(etype(PLAYER,?,?)*btype(INTEGER,?,?)),No_type);Type(shipLocations) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),etype(PLAYER,?,?));Type(playerShoots) == Cst(etype(MESSAGE,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(deployFleet) == Cst(etype(MESSAGE,?,?),etype(PLAYER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  Observers(Machine(Battleship)) == (Type(gameStatus) == Cst(etype(STATUS,?,?),No_type);Type(shotsTaken) == Cst(btype(INTEGER,?,?),etype(PLAYER,?,?));Type(shipLeft) == Cst(SetOf(etype(PLAYER,?,?)*btype(INTEGER,?,?)),No_type);Type(shipLocations) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)),etype(PLAYER,?,?)))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
