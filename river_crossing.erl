-module(river_crossing).

-export([cross_river/1]).

-define(RIVER, $~).
-define(PREDATORS, [{"cg", "Grain"}, {"dc", "Chicken"}]).

cross_river(Simulation) ->
  [Position | Moves] = position_and_moves(Simulation),
  cross_river(Position, Moves).

cross_river(Position, _Moves = []) ->
    Riverbanks = riverbanks(Position),
    io:format("Riverbanks: ~p, ~p~n", Riverbanks),
    PreyString  = case eaten(Riverbanks) of
        [] ->
            [];
        Prey ->
            Prey ++ "\n"
    end,
    Position ++ "\n" ++ PreyString;

cross_river(Position, [Move | Moves]) ->
    Riverbanks = riverbanks(Position),
    case eaten(Riverbanks) of
        [] ->
            cross_river(apply_move(Riverbanks, Move), Moves);
        Eaten ->
            Position ++ "\n" ++ Eaten ++ " was eaten.\n"
    end.

riverbanks([$~ | Items]) ->
    [[], Items];

riverbanks(Items) ->
    case lists:reverse(Items) of
        [$~ | LeftBank] ->
            [LeftBank, []];
        _ ->
            string:tokens(Items, "~")
    end.

eaten([]) -> [];
eaten([Riverbank | Riverbanks]) ->
    case prey(Riverbank, ?PREDATORS) of
        [] ->
            eaten(Riverbanks);
        Prey->
            Prey
    end.

prey(_, []) -> [];
prey(Riverbank, [{[Predator, Prey], PreyName} | Predators]) ->
    case not farmer(Riverbank) andalso has_items(Riverbank, [Predator, Prey]) of
        true ->
            PreyName;
        false ->
            prey(Riverbank, Predators)
    end.

farmer([]) -> false;
farmer([$f | _]) -> true;
farmer([_ | Items]) -> farmer(Items).

apply_move([LeftBank, RightBank], [$< | Movers]) ->
    case(has_items(RightBank, Movers)) of
      true ->
          io:format("Moving ~p to ~p from ~p~n", [Movers, LeftBank, RightBank]),
          LeftBank ++ Movers ++ [?RIVER] ++ remove_items(RightBank, Movers);
      false ->
          invalid_move
    end;

apply_move([LeftBank, RightBank], Move) ->
    [$> | Movers] = lists:reverse(Move),
    case(has_items(LeftBank, Movers)) of
      true ->
          io:format("Moving ~p to ~p from ~p~n", [Movers, RightBank, LeftBank]),
          remove_items(LeftBank, Movers) ++ [?RIVER] ++ Movers ++ RightBank;
      false ->
          invalid_move
    end.

has_items(Riverbank, Items) ->
    RiverbankSet = sets:from_list(Riverbank),
    ItemSet = sets:from_list(Items),
    MoversOnRiverbank = sets:to_list(sets:intersection(RiverbankSet, ItemSet)),
    lists:sort(MoversOnRiverbank) =:= lists:sort(Items).

remove_items(Riverbank, []) -> Riverbank;
remove_items(Riverbank, [Item | Items]) ->
    remove_items(lists:filter(fun(X) when X =:= Item -> true; (_) -> false end, Riverbank), Items).

position_and_moves(Simulation) ->
  string:tokens(Simulation, [$\n]).
