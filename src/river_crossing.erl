-module(river_crossing).

-export([cross_river/1]).

-include_lib("eunit/include/eunit.hrl").

-define(RIVER, $~).
-define(PREDATORS, [{"cg", "Grain"}, {"dc", "Chicken"}]).

cross_river(Simulation) ->
  [Position | Moves] = position_and_moves(Simulation),
  Riverbanks = riverbanks(Position),
  cross_river(Riverbanks, Moves).

cross_river(Riverbanks, _Moves = []) ->
    io:format("Riverbanks: ~p, ~p~n", Riverbanks),
    PreyString  = case eaten(Riverbanks) of
        [] ->
            [];
        Prey ->
            Prey ++ " was eaten.\n"
    end,
    position(Riverbanks) ++ "\n" ++ PreyString;

cross_river(Riverbanks, [Move | Moves]) ->
    case eaten(Riverbanks) of
        [] ->
            cross_river(apply_move(Riverbanks, Move), Moves);
        Eaten ->
            position(Riverbanks) ++ "\n" ++ Eaten ++ " was eaten.\n"
    end.

riverbanks([]) -> [[], []];
riverbanks([$~ | Items]) -> [[], Items];
riverbanks(Items) ->
    case lists:reverse(Items) of
        [$~ | LeftBank] ->
            [LeftBank, []];
        _ ->
            string:tokens(Items, "~")
    end.

position([Leftbank, Rightbank]) ->
    Leftbank ++ [?RIVER] ++ Rightbank.

eaten(Riverbanks) ->
    eaten(Riverbanks, ?PREDATORS).

eaten([], _) -> [];
eaten([Riverbank | Riverbanks], Predators) ->
    case prey(Riverbank, Predators) of
        [] ->
            eaten(Riverbanks, Predators);
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
          [LeftBank ++ Movers, remove_items(RightBank, Movers)];
      false ->
          invalid_move
    end;

apply_move([LeftBank, RightBank], Move) ->
    [$> | Movers] = lists:reverse(Move),
    case(has_items(LeftBank, Movers)) of
      true ->
          io:format("Moving ~p to ~p from ~p~n", [Movers, RightBank, LeftBank]),
          [remove_items(LeftBank, Movers), Movers ++ RightBank];
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
    remove_items(lists:filter(fun(X) when X =:= Item -> false; (_) -> true end, Riverbank), Items).

position_and_moves(Simulation) ->
  string:tokens(Simulation, [$\n]).

%% ---- TESTS ----

position_and_moves_test_() ->
    [?_assertEqual(["fdcg~", "fc>", "<f"], position_and_moves("fdcg~\nfc>\n<f")),
     ?_assertEqual(["no_moves"], position_and_moves("no_moves"))].

remove_items_test_() ->
    [?_assertEqual("abc", remove_items("abcde", "de")),
     ?_assertEqual("cde", remove_items("abcde", "ab")),
     ?_assertEqual("ade", remove_items("abcde", "bc")),
     ?_assertEqual("bcd", remove_items("abcde", "ae")),
     ?_assertEqual("", remove_items("abcde", "abcde")),
     ?_assertEqual("abcde", remove_items("abcde", "")),
     ?_assertEqual("abde", remove_items("abcde", "ccc")),
     ?_assertEqual("abcde", remove_items("abcde", "z"))].

has_items_test_() ->
    [?_assertEqual(true, has_items("abcde", "a")),
     ?_assertEqual(true, has_items("abcde", "ab")),
     ?_assertEqual(true, has_items("abcde", "ae")),
     ?_assertEqual(false, has_items("abcde", "aa")),
     ?_assertEqual(true, has_items("abcde", "")),
     ?_assertEqual(true, has_items("abcde", "abcde")),
     ?_assertEqual(true, has_items("abcde", "edcba")),
     ?_assertEqual(false, has_items("abcde", "f"))].

apply_move_test_() ->
    [?_assertEqual(["dg", "cf"], apply_move(["fcdg", []], "fc>")),
     ?_assertEqual(["fc", "dg"], apply_move([[], "fcdg"], "<fc")),
     ?_assertEqual(["dgf", "c"], apply_move(["dg", "fc"], "<f")),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], "<fc")),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], "ff>")),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], "xy>"))].

farmer_test_() ->
    [?_assertEqual(true, farmer("abcdef")),
     ?_assertEqual(true, farmer("fedcba")),
     ?_assertEqual(true, farmer("defgh")),
     ?_assertEqual(true, farmer("f")),
     ?_assertEqual(false, farmer("a"))].

prey_test_() ->
    [?_assertEqual([], prey("", [])),
     ?_assertEqual([], prey("ab", [{"ac", "Chuck Norris"}])),
     ?_assertEqual([], prey("ab", [{"ac", "Chuck Norris"}, {"ae", "Ed Sullivan"}])),
     ?_assertEqual("Bob", prey("ab", [{"ab", "Bob"}])),
     ?_assertEqual("Bob", prey("ab", [{"ab", "Bob"}, {"ac", "Chuck Norris"}])),
     ?_assertEqual("Bob", prey("ab", [{"ac", "Chuck Norris"}, {"ab", "Bob"}]))].

eaten_test_() ->
    [?_assertEqual([], eaten([])),
     ?_assertEqual([], eaten([], [])),
     ?_assertEqual([], eaten(["cf", []])),
     ?_assertEqual([], eaten([[], "cf"])),
     ?_assertEqual([], eaten(["cfg", []])),
     ?_assertEqual("Grain", eaten(["gc", []])),
     ?_assertEqual("Chicken", eaten(["abcdekz", []])),
     ?_assertEqual("Grain", eaten(["cg", []])),
     ?_assertEqual("Grain", eaten([[], "cg"])),
     ?_assertEqual("Grain", eaten(["cg", []], [{"cg", "Grain"}])),
     ?_assertEqual("Chicken", eaten([[], "cg"], [{"cg", "Chicken"}])),
     ?_assertEqual("Grain", eaten(["cg", []], [{"cg", "Grain"}, {"dc", "Chicken"}])),
     ?_assertEqual("Grain", eaten(["cg", []], [{"dc", "Chicken"}, {"cg", "Grain"}]))].

riverbanks_test_() ->
    [?_assertEqual([[], []], riverbanks([])),
     ?_assertEqual([[], []], riverbanks("~")),
     ?_assertEqual(["a", []], riverbanks("a~")),
     ?_assertEqual(["ba", []], riverbanks("ab~")),
     ?_assertEqual([[], "a"], riverbanks("~a")),
     ?_assertEqual([[], "ab"], riverbanks("~ab")),
     ?_assertEqual(["a", "b"], riverbanks("a~b")),
     ?_assertEqual(["a", "bc"], riverbanks("a~bc"))].

position_test_() ->
    [?_assertEqual("~", position(["", ""])),
     ?_assertEqual("a~", position(["a", ""])),
     ?_assertEqual("~a", position(["", "a"])),
     ?_assertEqual("a~b", position(["a", "b"]))].

cross_river_test_() ->
    [{"Successful crossing",
      ?_assertEqual("~cfdg\n",
                    cross_river("fcdg~\nfc>\n<f\nfg>\n<fc\nfd>\n<f\nfc>\n"))},
     {"Grain eaten",
      ?_assertEqual("gc~df\nGrain was eaten.\n",
                    cross_river("fcdg~\nfd>\n"))},
     {"Grain eaten right off the bat",
      ?_assertEqual("cg~fd\nGrain was eaten.\n",
                    cross_river("cg~~fd\n<f"))}].
