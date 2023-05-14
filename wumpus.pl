:- dynamic([time/1,isWumpus/1,isEnergy/1,isGold/1,isWeapon/1,isLight/1,isExit/1,visited_cells/1,power/1,capacity/1,world_rows/1,world_cols/1,energy_locations/1,gold_locations/1,light_locations/1,weapon_locations/1,exit_locations/1,wumpus_locations/1]).

start :-
    init,
    step.
    
init :-
    init_game,
    init_land,
    init_agent.
    
step :-
    agent_location(AL),
    AL = [X,Y],
    format('\ncurrent pos : ~p,~p~n',[X,Y]),
    power(P),
    format('power : ~p~n',[P]),
    write('before perceptions :\n'),
    (leftHand(empty) -> LH is 0,write('his left hand is empty\n');(leftHand(weapon) -> LH is 1,write('there is a weapon in his left hand\n');LH is 2,write('there is a gold in his left hand\n') ) ),
    (rightHand(empty) -> RH is 0,write('his right hand is empty\n');(rightHand(weapon) -> RH is 1,write('there is a weapon in his right hand\n');RH is 2,write('there is a gold in his right hand\n'))),
    make_percept([Odour,Gold,Weapon,Energy,Exit,Wumpus]),
    write('perceptions:\n'),
    (light_location(LL),LL=AL->write('there is a light here\n');write('there is no light here\n')),
    (Weapon = yes -> write('there is a weapon here\n');write('there is no weapon here\n')),
    (Gold = yes -> write('there is a gold here\n');write('there is no gold here\n')),
    (Energy = yes -> write('there is some energy here\n');write('there is no energy here\n')),
    (Wumpus = yes -> write('there is a wumpus here\n');write('there is no wumpus here\n')),
    (Odour = yes -> write('the wumpus odour can be heard here\n');write('there is no odour present here\n')),
    (Exit = yes -> write('there is a exist here\n');write('there is no exit here\n')),
    visited_cells(VC),
    append([AL],VC,NVC),
    retractall(visited_cells(_)),
    asserta(visited_cells(NVC)),
    handle_percept([Odour,Gold,Weapon,Energy,Exit,Wumpus]),
    write('after perceptions :\n'),
    (leftHand(empty) -> NLH is 0,write('your left hand is empty\n');(leftHand(weapon) -> NLH is 1,write('there is a weapon in your left hand\n');NLH is 2,write('there is a gold in left hand\n') ) ),
    (rightHand(empty) -> NRH is 0,write('your right hand is empty\n');(rightHand(weapon) -> NRH is 1,write('there is a weapon in your right hand\n');NRH is 2,write('there is a gold in your right hand\n'))),
    power(NP),
    (Exit=yes -> (answer),write('he succeeded.\n'),true;(do_action(Action),step -> true;write('he failed to answer the question.\n'))).

answer:-
    question(X,L),
    answer(X,L).
    
answer(gold,L):- isGold(yes,L).
answer(gold,L):- isGold(no,L).

answer(wumpus,L):- isWumpus(yes,L).
answer(wumpus,L):- isWumpus(no,L).

answer(energy,L):- isEnergy(yes,L).
answer(energy,L):- isEnergy(no,L).

answer(exit,L):- isExit(yes,L).
answer(exit,L):- isExit(no,L).

answer(weapon,L):- isWeapon(yes,L).
answer(weapon,L):- isWeapon(no,L).


init_game :-
        
    retractall( isWumpus(_,_) ),
    retractall( isEnergy(_,_) ),
    retractall( isGold(_,_) ),
    retractall( isWeapon(_,_)),
    retractall( isLight(_,_) ),
    retractall( isExit(_,_) ),
    
    retractall( visited_cells(_) ),
    asserta( visited_cells([]) ).
    
init_land :-
    
    retractall( power(_) ),
    retractall( capacity(_)),
    retractall( world_rows(_)),
    retractall( world_cols(_)),
    retractall( energy_locations(_)),
    retractall( gold_locations(_)),
    retractall( light_locations(_)),
    retractall( weapon_locations(_)),
    retractall( exit_locations(_)),
    retractall( wumpus_locations(_)),
    consult('data.db'),
    retractall( energy_location(_)),
    energy_locations(X1),
    insert_energy_location(X1),
    retractall( gold_location(_)),
    gold_locations(X2),
    insert_gold_location(X2),
    retractall( light_location(_)),
    light_locations(X3),
    insert_light_location(X3),
    retractall( weapon_location(_)),
    weapon_locations(X4),
    insert_weapon_location(X4),
    retractall( exit_location(_)),
    exit_locations(X5),
    insert_exit_location(X5),
    retractall( wumpus_location(_)),
    wumpus_locations(X6),
    insert_wumpus_location(X6).

%add energy locations to energy_location
insert_energy_location(Z):-
    Z = [],!.
insert_energy_location(Z):-
    Z = [X|Y],
    asserta(energy_location(X)),
    insert_energy_location(Y).

%add gold locations to gold_location
insert_gold_location(Z):-
    Z = [],!.
insert_gold_location(Z):-
    Z = [X|Y],
    asserta(gold_location(X)),
    insert_gold_location(Y).
    
%add light locations to light_location
insert_light_location(Z):-
    Z = [],!.
insert_light_location(Z):-
    Z = [X|Y],
    asserta(light_location(X)),
    insert_light_location(Y).
    
%add weapon locations to weapon_location
insert_weapon_location(Z):-
    Z = [],!.
insert_weapon_location(Z):-
    Z = [X|Y],
    asserta(weapon_location(X)),
    insert_weapon_location(Y).
    
%add exit locations to exit_location
insert_exit_location(Z):-
    Z = [],!.
insert_exit_location(Z):-
    Z = [X|Y],
    asserta(exit_location(X)),
    insert_exit_location(Y).
    
%add wumpus locations to wumpus_location
insert_wumpus_location(Z):-
    Z = [],!.
insert_wumpus_location(Z):-
    Z = [X|Y],
    asserta(wumpus_location(X)),
    insert_wumpus_location(Y).
    
init_agent :-
    
    retractall( leftHand(_)),
    retractall( rightHand(_)),
    asserta( leftHand(empty) ),
    asserta( rightHand(empty) ),
    retractall( agent_location(_) ),
    asserta( agent_location([1,1])).
    
enter_room(L) :-
    light_location(LL),
    LL = L,!.
enter_room(L) :-
    rightHand(gold),!.
enter_room(L) :-
    leftHand(gold),!.
enter_room(L) :-
    visited_cells(VC),
    member(L,VC),
    !.

    
make_percept([Odour,Gold,Weapon,Energy,Exit,Wumpus]):-
    is_hearing_shout(Odour),
    is_seeing_gold(Gold),
    is_seeing_weapon(Weapon),
    is_seeing_energy(Energy),
    is_seeing_exit(Exit),
    is_seeing_wumpus(Wumpus).
    
is_hearing_shout(yes) :-
    agent_location(X),%agent_location([1,1]).
    wumpus_location(Y),
    adjacent(X,Y),!.
is_hearing_shout(no).

is_seeing_gold(yes) :-
    agent_location(X),
    gold_location(Y),
    X = Y,!.
is_seeing_gold(no).

is_seeing_weapon(yes):-
    agent_location(X),
    weapon_location(Y),
    X = Y,!.
is_seeing_weapon(no).

is_seeing_energy(yes) :-
    agent_location(X),
    energy_location(Y),
    X=Y,!.
is_seeing_energy(no).

is_seeing_exit(yes) :-
    agent_location(X),
    exit_location(Y),
    X=Y,!.
is_seeing_exit(no).

is_seeing_wumpus(yes) :-
    agent_location(X),
    wumpus_location(Y),
    X=Y,!.
is_seeing_wumpus(no).

    

    
handle_percept([Odour,Gold,Weapon,Energy,Exit,Wumpus]):-
    handle_shout(Odour),
    handle_gold(Gold),
    handle_weapon(Weapon),
    handle_energy(Energy),
    handle_exit(Exit),
    handle_wumpus(Wumpus).

%handle odour
handle_shout(no):-
    agent_location([X,Y]),
    Z1 is Y+1,
    Z2 is Y-1,
    Z3 is X+1,
    Z4 is X-1,
    
    retractall( isWumpus(_, [X,Z1]) ),
    asserta( isWumpus(no, [X,Z1]) ),
    retractall( isWumpus(_, [X,Z2]) ),
    asserta( isWumpus(no, [X,Z2]) ),
    retractall( isWumpus(_, [Z3,Y]) ),
    asserta( isWumpus(no, [Z3,Y]) ) ,
    retractall( isWumpus(_, [Z4,Y]) ),
    asserta( isWumpus(no, [Z4,Y])).
    
handle_shout(yes):-
    agent_location([X,Y]),
    Z1 is Y+1,
    Z2 is Y-1,
    Z3 is X+1,
    Z4 is X-1,
    (isWumpus(no,[X,Z1])->true;retractall(isWumpus(_,[X,Z1])),asserta(isWumpus(yes,[X,Z1]))),
    (isWumpus(no,[X,Z2])->true;retractall(isWumpus(_,[X,Z2])),asserta(isWumpus(yes,[X,Z2]))),
    (isWumpus(no,[Z3,Y])->true;retractall(isWumpus(_,[Z3,Y])),asserta(isWumpus(yes,[Z3,Y]))),
    (isWumpus(no,[Z4,Y])->true;retractall(isWumpus(_,[Z4,Y])),asserta(isWumpus(yes,[Z4,Y]))).
    
  
%handle gold
handle_gold(no):-
    agent_location(AL),
    retractall( isGold(_,AL)),
    asserta( isGold(no,AL)).
    
handle_gold(yes):-
    leftHand(empty),
    retractall(leftHand(empty)),
    asserta(leftHand(gold)),
    agent_location(AL),
    retractall(gold_location(AL)),
    retractall( isGold(_,AL)),
    asserta( isGold(yes,AL)),!.
    
handle_gold(yes):-
    rightHand(empty),
    retractall(rightHand(empty)),
    asserta(rightHand(gold)),
    agent_location(AL),
    retractall(gold_location(AL)),
    agent_location(AL),
    retractall( isGold(_,AL)),
    asserta( isGold(yes,AL)),!.
    
handle_gold(yes):-
    agent_location(AL),
    retractall( isGold(_,AL) ),
    asserta( isGold(yes,AL)),!.
    
%handle weapon
handle_weapon(no):-
    agent_location(AL),
    retractall( isWeapon(_,AL)),
    asserta( isWeapon(no,AL)).
    
handle_weapon(yes):-
    leftHand(empty),
    retractall(leftHand(empty)),
    asserta(leftHand(weapon)),
    agent_location(AL),
    retractall(weapon_location(AL)),
    agent_location(AL),
    retractall( isWeapon(_,AL) ),
    asserta( isWeapon(yes,AL)),!.
    
handle_weapon(yes):-
    rightHand(empty),
    retractall(rightHand(empty)),
    asserta(rightHand(weapon)),
    agent_location(AL),
    retractall(weapon_location(AL)),
    agent_location(AL),
    retractall( isWeapon(_,AL) ),
    asserta( isWeapon(yes,AL)),!.
    
handle_weapon(yes):-
    agent_location(AL),
    retractall( isWeapon(_,AL) ),
    asserta( isWeapon(yes,AL)),!.
    
%handle energy
handle_energy(no):-
    agent_location(AL),
    retractall( isEnergy(_,AL)),
    asserta( isEnergy(no,AL)).
    
handle_energy(yes):-
    power(P),
    capacity(C),
    P + 2 < C + 1,
    retractall(power(_)),
    NewP is P+2,
    retractall(power(P)),
    asserta(power(NewP)),
    agent_location(AL),
    retractall( isEnergy(_,AL) ),
    asserta( isEnergy(yes,AL)),!.
    
handle_energy(yes):-
    agent_location(AL),
    retractall( isEnergy(_,AL) ),
    asserta( isEnergy(yes,AL)),!.
    
%handle wumpus
handle_wumpus(yes):-
    leftHand(weapon),
    agent_location(AL),
    retractall(wumpus_location(AL)),
    retractall(isWumpus(_,AL)),
    asserta(isWumpus(yes,AL)),!.
    
handle_wumpus(yes):-
    rightHand(weapon),
    agent_location(AL),
    retractall(wumpus_location(AL)),
    retractall(isWumpus(_,AL)),
    asserta(isWumpus(yes,AL)),!.

handle_wumpus(no):-
    agent_location(AL),
    retractall( isWumpus(_, AL) ),
    asserta( isWumpus(no, AL) ).

%handle exit
handle_exit(no):-
    agent_location(AL),
    retractall( isExit(_, AL) ),
    asserta( isExit(no, AL) ).
    
handle_exit(yes):-
    agent_location(AL),
    retractall( isExit(_, AL) ),
    asserta( isExit(yes, AL) ).

%Action
do_action(Action) :-
    leftHand(weapon),
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    
do_action(Action) :-
    rightHand(weapon),
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    
do_action(Action) :-
    isWumpus(no,L),
    agent_location(AL),
    adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    not_member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    
do_action(Action) :-
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    member(L,VC),
    leftHand(weapon),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    
do_action(Action) :-
    agent_location(AL),
    find_adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    member(L,VC),
    rightHand(weapon),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    
do_action(Action) :-
    isWumpus(no,L),
    agent_location(AL),
    adjacent(AL,L),
    enter_room(L),
    visited_cells(VC),
    member(L,VC),
    permitted(L),
    power(P),
    consum_power(X),
    P - X > 0,
    retractall(agent_location(_)),
    asserta(agent_location(L)),
    NewP is P-X,
    retractall(power(_)),
    asserta(power(NewP)),
    Action = L,!.
    

    
consum_power(X):-
    leftHand(empty),
    rightHand(empty),
    X is 1,!.
  
consum_power(X):-
    leftHand(empty),
    not(rightHand(empty)),
    X is 2,!.
    
consum_power(X):-
    rightHand(empty),
    not(leftHand(empty)),
    X is 2,!.
    
consum_power(X):-
    not(leftHand(empty)),
    not(rightHand(empty)),
    X is 4,!.

    
adjacent([X2,Y2],[X1,Y1]):-
N is abs(X1-X2),M is abs(Y1-Y2),(N = 1,M = 0),!.
adjacent([X2,Y2],[X1,Y1]):-
N is abs(X1-X2),M is abs(Y1-Y2),(N = 0,M = 1),!.

find_adjacent([X,Y],[Z,W]):-
    Z is X+1,
    W is Y.
    
find_adjacent([X,Y],[Z,W]):-
    Z is X-1,
    W is Y.
    
find_adjacent([X,Y],[Z,W]):-
    Z is X,
    W is Y+1.

find_adjacent([X,Y],[Z,W]):-
    Z is X,
    W is Y-1.
    

permitted([X,Y]) :-
    world_rows(Row),
    world_rows(Col),
    0 < X, X < Row+1,
    0 < Y, Y < Col+1.
    

not_member(X, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
).

member(X, []):- fail.
member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> true
    ; member([X,Y], Ys)
).
