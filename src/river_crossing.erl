-module(river_crossing).

-export([solve_puzzle/1]).
-export([cross_river/1]).

-include_lib("eunit/include/eunit.hrl").

-define(RIVER, $~).

-record(predator, {predator :: char(),
                   prey :: char(),
                   prey_name :: string()}).

-record(move, {direction :: char(),
               items :: list()}).

solve_puzzle(GameInstructions) ->
    {Riverbanks, _, Predators} = parse(GameInstructions),
    solve_puzzle(Riverbanks, Predators, [Riverbanks]).

solve_puzzle([[], _] = Riverbanks, _, States) ->
    {solution, lists:reverse([Riverbanks | States])};

solve_puzzle(Riverbanks, Predators, States) ->
    case new_valid_move(Riverbanks, Predators, States) of
        [] ->
            {unsolvable, Riverbanks, States};
        Move ->
            solve_puzzle(apply_move(Riverbanks, Move), Predators, [Riverbanks | States])
    end.

new_valid_move(Riverbanks, Predators, States) ->
    valid_move(all_moves(Riverbanks), Riverbanks, Predators, States).

all_moves([LeftBank, RightBank]) ->
    case farmer(LeftBank) of
        true ->
            all_moves(right, LeftBank);
        false ->
            all_moves(left, RightBank)
   end.

all_moves(Direction, Riverbank) ->
    MoveFarmer = #move{direction = Direction, items = "f"},
    OtherItems = lists:filter(fun($f) -> false; (_) -> true end, Riverbank),
    OtherMoves = [#move{direction = Direction, items = [$f, Item]} || Item <- OtherItems],
    case Direction of
        right ->
            OtherMoves;
        left ->
            [MoveFarmer | OtherMoves]
    end.

valid_move([], _, _, _) -> [];
valid_move([Move | Moves], Riverbanks, Predators, States) ->
    NewRiverbanks = apply_move(Riverbanks, Move),
    case is_safe(NewRiverbanks, Predators) andalso is_new_state(NewRiverbanks, States) of
        true ->
            Move;
        false ->
            valid_move(Moves, Riverbanks, Predators, States)
    end.

is_safe([LeftBank, RightBank], Predators) ->
    [] == unsafe_prey(LeftBank, Predators) andalso
    [] == unsafe_prey(RightBank, Predators).

is_new_state([NewLeftBank, _], ExistingRiverbanks) ->
    SortedNewBank = lists:sort(NewLeftBank),
    RiverbanksEqual = fun([OldLeftBank, _]) ->
                          SortedNewBank == lists:sort(OldLeftBank)
                      end,
    [] == lists:filter(RiverbanksEqual, ExistingRiverbanks).

cross_river(GameInstructions) ->
  {Riverbanks, Moves, Predators} = parse(GameInstructions),
  cross_river(Riverbanks, Moves, Predators).

cross_river(Riverbanks, _Moves = [], Predators) ->
    PreyString  = case eaten(Riverbanks, Predators) of
        [] ->
            [];
        Prey ->
            Prey ++ " was eaten.\n"
    end,
    river_desc(Riverbanks) ++ "\n" ++ PreyString;

cross_river(Riverbanks, [Move | Moves], Predators) ->
    case eaten(Riverbanks, Predators) of
        [] ->
            cross_river(apply_move(Riverbanks, Move), Moves, Predators);
        Eaten ->
            river_desc(Riverbanks) ++ "\n" ++ Eaten ++ " was eaten.\n"
    end.

river_desc([Leftbank, Rightbank]) ->
    Leftbank ++ [?RIVER] ++ Rightbank.

eaten([], _) -> [];
eaten([Riverbank | Riverbanks], Predators) ->
    case unsafe_prey(Riverbank, Predators) of
        [] ->
            eaten(Riverbanks, Predators);
        Prey->
            Prey
    end.

unsafe_prey(_, []) -> [];
unsafe_prey(Riverbank, [#predator{predator = Predator,
                                  prey = Prey,
                                  prey_name = PreyName} | Predators]) ->
    case not farmer(Riverbank) andalso has_items(Riverbank, [Predator, Prey]) of
        true ->
            PreyName;
        false ->
            unsafe_prey(Riverbank, Predators)
    end.

farmer([]) -> false;
farmer([$f | _]) -> true;
farmer([_ | Items]) -> farmer(Items).

apply_move([LeftBank, RightBank], #move{direction = left, items = Items}) ->
    case(has_items(RightBank, Items)) of
      true ->
          [LeftBank ++ Items, remove_items(RightBank, Items)];
      false ->
          invalid_move
    end;

apply_move([LeftBank, RightBank], #move{direction = right, items = Items}) ->
    case(has_items(LeftBank, Items)) of
      true ->
          [remove_items(LeftBank, Items), Items ++ RightBank];
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

parse(GameInstructions) ->
  [River | MovesAndPredators] = string:tokens(GameInstructions, [$\n]),
  Riverbanks = riverbanks(River),
  Moves = moves(MovesAndPredators),
  Predators = predators(MovesAndPredators),
  {Riverbanks, Moves, Predators}.

riverbanks([]) -> [[], []];
riverbanks([$~ | Items]) -> [[], Items];
riverbanks(Items) ->
    case lists:reverse(Items) of
        [$~ | LeftBank] ->
            [LeftBank, []];
        _ ->
            string:tokens(Items, "~")
    end.

moves(Instructions) ->
    SortedInstructions = lists:map(fun lists:sort/1, Instructions),
    lists:reverse(lists:foldl(fun parse_move/2, [], SortedInstructions)).

parse_move([$< | Items], Moves) ->
    [#move{direction = left, items = Items} | Moves];

parse_move([$> | Items], Moves) ->
    [#move{direction = right, items = Items} | Moves];

parse_move(_, Moves) ->
    Moves.

predators(Instructions) ->
    Predators = lists:foldl(fun parse_predator/2, [], Instructions),
    case Predators of
        [] ->
            default_predators();
        _ ->
            Predators
    end.

parse_predator(Instruction, Predators) ->
    case string:tokens(Instruction, " ") of
        [[Predator], "eats", PreyWithName] ->
            [predator(Predator, PreyWithName) | Predators];
        _ ->
            Predators
    end.

predator(Predator, PreyWithName) ->
    {[PreyChar], PreyName} = case string:tokens(PreyWithName, ",") of
        [Prey, Name] ->
           {Prey, Name};
        [Prey] ->
           {Prey, Prey}
    end,
    #predator{predator = Predator,
              prey = PreyChar,
              prey_name = PreyName}.

default_predators() ->
    [#predator{predator = $d,
               prey = $c,
               prey_name = "Chicken"},
     #predator{predator = $c,
               prey = $g,
               prey_name = "Grain"}].

%% ---- TESTS ----

predator_test_() ->
    [{"Predator without prey name",
      fun() ->
          Predator = #predator{predator = $a,
                               prey = $b,
                               prey_name = "b"},
          ?assertEqual(Predator, predator($a, "b"))
      end},
     {"Predator with missing prey name (i.e. trailing comma)",
      fun() ->
          Predator = #predator{predator = $a,
                               prey = $b,
                               prey_name = "b"},
          ?assertEqual(Predator, predator($a, "b,"))
      end},
     {"Predator with prey name",
      fun() ->
          Predator = #predator{predator = $a,
                               prey = $b,
                               prey_name = "Buffalo"},
          ?assertEqual(Predator, predator($a, "b,Buffalo"))
      end}].

parse_predator_test_() ->
    [{"Not specified or missing name (i.e. trailing comma)",
      fun() ->
          Predator = #predator{predator = $a,
                               prey = $b,
                               prey_name = "b"},
          ?assertEqual([Predator], parse_predator("a eats b", [])),
          ?assertEqual([Predator], parse_predator("a eats b,", []))
      end},
     {"Name specified",
      fun() ->
          Predator = #predator{predator = $a,
                               prey = $b,
                               prey_name = "Barnacle_Bill"},
          ?assertEqual([Predator], parse_predator("a eats b,Barnacle_Bill", []))
      end}].

parse_move_test_() ->
    [{"Move items left",
      fun() ->
          Move = #move{direction = left,
                           items = "ab"},
          ?assertEqual([Move], parse_move("<ab", []))
      end},
     {"Move items right",
      fun() ->
          Move = #move{direction = right,
                           items = "ab"},
          ?assertEqual([Move], parse_move(">ab", []))
      end},
     {"Not a valid move (at this point in the code)",
      fun() ->
          ?assertEqual([], parse_move("a>b", []))
      end},
     {"Not a valid move",
      fun() ->
          ?assertEqual([], parse_move("ab", []))
      end}].

moves_test_() ->
    [{"No valid moves",
      ?_assertEqual([], moves([]))},
     {"Valid move",
      fun() ->
          Move = #move{direction = right,
                       items = "ab"},
          ?assertEqual([Move], moves(["ab>"])),
          ?assertEqual([Move], moves(["ab>", "not a move"])),
          ?assertEqual([Move], moves([">ab"])),
          ?assertEqual([Move], moves(["a>b"]))
      end},
     {"Multiple moves",
      fun() ->
          Move1 = #move{direction = right,
                        items = "ab"},
          Move2 = #move{direction = left,
                        items = "cd"},
          ?assertEqual([Move1, Move2], moves(["ab>", "<cd"]))
      end}].

riverbanks_test_() ->
    [?_assertEqual([[], []], riverbanks([])),
     ?_assertEqual([[], []], riverbanks("~")),
     ?_assertEqual(["a", []], riverbanks("a~")),
     ?_assertEqual(["ba", []], riverbanks("ab~")),
     ?_assertEqual([[], "a"], riverbanks("~a")),
     ?_assertEqual([[], "ab"], riverbanks("~ab")),
     ?_assertEqual(["a", "b"], riverbanks("a~b")),
     ?_assertEqual(["a", "bc"], riverbanks("a~bc"))].

parse_test_() ->
    [{"No items, no moves, no predators",
      fun() ->
          Expected= {[[], []], [], default_predators()},
          ?assertEqual(Expected, parse("~\n"))
      end},
     {"Items but no moves or predators",
      fun() ->
          Expected= {["a", "b"], [], default_predators()},
          ?assertEqual(Expected, parse("a~b\n"))
      end}].

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
    FarmerChickenRight = #move{direction = right, items = "fc"},
    FarmerChickenLeft = #move{direction = left, items = "fc"},
    FarmerLeft = #move{direction = left, items = "f"},
    FarmerFarmerRight = #move{direction = right, items = "ff"},
    XYRight = #move{direction = right, items = "xy"},
    [?_assertEqual(["dg", "fc"], apply_move(["fcdg", []], FarmerChickenRight)),
     ?_assertEqual(["fc", "dg"], apply_move([[], "fcdg"], FarmerChickenLeft)),
     ?_assertEqual(["dgf", "c"], apply_move(["dg", "fc"], FarmerLeft)),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], FarmerChickenLeft)),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], FarmerFarmerRight)),
     ?_assertEqual(invalid_move, apply_move(["fcdg", []], XYRight))].

farmer_test_() ->
    [?_assertEqual(true, farmer("abcdef")),
     ?_assertEqual(true, farmer("fedcba")),
     ?_assertEqual(true, farmer("defgh")),
     ?_assertEqual(true, farmer("f")),
     ?_assertEqual(false, farmer("a"))].

unsafe_prey_test_() ->
    PredatorAb = predator($a, "b,Bob"),
    PredatorAc = predator($a, "c"),
    PredatorAe = predator($a, "e"),
    [?_assertEqual([], unsafe_prey("", [])),
     ?_assertEqual([], unsafe_prey("ab", [PredatorAc])),
     ?_assertEqual([], unsafe_prey("ab", [PredatorAc, PredatorAe])),
     ?_assertEqual("Bob", unsafe_prey("ab", [PredatorAb])),
     ?_assertEqual("Bob", unsafe_prey("ab", [PredatorAb, PredatorAc])),
     ?_assertEqual("Bob", unsafe_prey("ab", [PredatorAc, PredatorAb]))].

eaten_test_() ->
    ChickenGrain = predator($c, "g,Grain"),
    GrainChicken = predator($g, "c,Chicken"),
    DogChicken = predator($d, "c,Chicken"),
    [?_assertEqual([], eaten([], default_predators())),
     ?_assertEqual([], eaten([], [])),
     ?_assertEqual([], eaten(["cf", []], default_predators())),
     ?_assertEqual([], eaten([[], "cf"], default_predators())),
     ?_assertEqual([], eaten(["cfg", []], default_predators())),
     ?_assertEqual("Grain", eaten(["gc", []], default_predators())),
     ?_assertEqual("Chicken", eaten(["abcdekz", []], default_predators())),
     ?_assertEqual("Grain", eaten(["cg", []], default_predators())),
     ?_assertEqual("Grain", eaten([[], "cg"], default_predators())),
     ?_assertEqual("Grain", eaten(["cg", []], [ChickenGrain])),
     ?_assertEqual("Chicken", eaten([[], "cg"], [GrainChicken])),
     ?_assertEqual("Grain", eaten(["cg", []], [ChickenGrain, DogChicken])),
     ?_assertEqual("Grain", eaten(["cg", []], [DogChicken, ChickenGrain]))].

river_desc_test_() ->
    [?_assertEqual("~", river_desc(["", ""])),
     ?_assertEqual("a~", river_desc(["a", ""])),
     ?_assertEqual("~a", river_desc(["", "a"])),
     ?_assertEqual("a~b", river_desc(["a", "b"]))].

cross_river_test_() ->
    [{"Successful crossing",
      ?_assertEqual("~cfdg\n",
                    cross_river("fcdg~\nfc>\n<f\nfg>\n<fc\nfd>\n<f\nfc>\n"))},
     {"Run out of moves, but nothing is eaten",
      ?_assertEqual("gd~cf\n",
                    cross_river("fcdg~\nfc>\n"))},
     {"Allow redundant moves (perhaps for artistic or poetic reasons?)",
      ?_assertEqual("gd~cf\n",
                    cross_river("fcdg~\nfc>\n<fc\nfc>"))},
     {"Grain eaten",
      ?_assertEqual("gc~df\nGrain was eaten.\n",
                    cross_river("fcdg~\nfd>\n"))},
     {"Grain eaten right off the bat",
      ?_assertEqual("cg~fd\nGrain was eaten.\n",
                    cross_river("cg~~fd\n<f"))},
     {"Chicken and dog moved",
      ?_assertEqual("g~dfc\n",
                    cross_river("fdcg~\nfc>\n<f\nfd>\n"))},
     {"Chicken and dog moved",
      ?_assertEqual("g~dfc\n",
                    cross_river("fdcg~\nfc>\n<f\nfd>\n"))},
     {"Dragon eats unicorn",
      ?_assertEqual("ud~cf\nUnicorn was eaten.\n",
                    cross_river("ducf~\nd eats u,Unicorn\nu eats c,Candy_Cane\nfc>\n"))}].

is_new_state_test_() ->
    [?_assert(is_new_state(["", ""], [["a", ""]])),
     ?_assert(is_new_state(["a", ""], [["", ""]])),
     ?_assert(is_new_state(["a", ""], [["b", ""]])),
     ?_assert(is_new_state(["a", ""], [["b", ""], ["c", ""]])),
     ?_assertNot(is_new_state(["a", ""], [["a", ""]])),
     ?_assertNot(is_new_state(["a", ""], [["b", ""], ["a", ""]])),
     ?_assertNot(is_new_state(["a", ""], [["a", ""], ["b", ""]]))].

is_safe_test_() ->
    Predators = default_predators(),
    Safe = "dg",
    Unsafe = "cg",
    io:format(user, "is_safe_test~n", []),
    [?_assert(is_safe([Safe, Safe], Predators)),
     ?_assertNot(is_safe([Unsafe, Safe], Predators)),
     ?_assertNot(is_safe([Safe, Unsafe], Predators))].

valid_move_test_() ->
    Predators = default_predators(),
    MoveFarmerRight = #move{direction = right, items = "f"},
    MoveFarmerDogRight = #move{direction = right, items = "fd"},
    OldState = [["d", "f"]],
    [?_assertEqual([], valid_move([], nil, nil, nil)),
     ?_assertEqual([], valid_move([MoveFarmerRight], ["dcf", ""], Predators, [])),
     ?_assertEqual([], valid_move([MoveFarmerRight], ["df", ""], Predators, OldState)),
     ?_assertEqual(MoveFarmerRight, valid_move([MoveFarmerRight], ["df", ""], Predators, [])),
     ?_assertEqual(MoveFarmerDogRight,
                  valid_move([MoveFarmerRight, MoveFarmerDogRight],
                             ["dcf", ""], Predators, []))].

all_moves_test_() ->
    FDR = #move{direction = right, items = "fd"},
    FCR = #move{direction = right, items = "fc"},
    FL = #move{direction = left, items = "f"},
    [?_assertEqual([FDR, FCR], all_moves(right, "fdc")),
     ?_assertEqual([FDR], all_moves(right, "fd")),
     ?_assertEqual([], all_moves(right, "f")),
     ?_assertEqual([FL], all_moves(left, "f")),
     ?_assertEqual([FDR, FCR], all_moves(["fdc", nil])),
     ?_assertEqual([FDR], all_moves(["fd", nil])),
     ?_assertEqual([], all_moves(["f", nil])),
     ?_assertEqual([FL], all_moves(["", "f"]))].

new_valid_move_test_() ->
    FL = #move{direction = left, items = "f"},
    FDR = #move{direction = right, items = "fd"},
    Predators = default_predators(),
    OldState = [["", nil]],
    OldStates = [["d", nil], ["", nil]],
    [ ?_assertEqual([], new_valid_move(["fd", ""], Predators, OldStates)),
     ?_assertEqual([], new_valid_move(["f", ""], Predators, OldState)),
     {"Farmer never needs to go from left to right empty-handed",
      [?_assertEqual([], new_valid_move(["f", ""], Predators, [])),
       ?_assertEqual([], new_valid_move(["f", "d"], Predators, [["f", nil]]))]},
     ?_assertEqual(FL, new_valid_move(["", "f"], Predators, [["", "f"]])),
     ?_assertEqual(FDR, new_valid_move(["fdc", ""], Predators, [])),
     {"Ruthless farmer shoots one dog (duplicates get removed when move from a bank)",
      ?_assertEqual(FDR, new_valid_move(["fdcdc", "dc"], Predators, []))}].

solve_puzzle_test_() ->
    Predators = default_predators(),
    [{"No safe moves: dog is omnivore, chicken eats grain",
      fun() ->
          GameInstructions = "fdcg~\nd eats c\nd eats g\nc eats g\n",
          {unsolvable, _, _} = solve_puzzle(GameInstructions)
      end},
     {"No unused moves",
      fun() ->
          OldStates = [["dcg", nil],
                       ["cg", nil],
                       ["dg", nil],
                       ["dc", nil]],
          {unsolvable, _, _} = solve_puzzle(["fdcg", ""], Predators, OldStates)
      end},
     {"Solvable with fc>, <f, fg>, <fc, fd>, <f, fc>",
      fun() ->
          {solution, _} = solve_puzzle(["fdcg", ""], Predators, []),
          {solution, _} = solve_puzzle("fdcg~\n")
      end},
     {"Solvable by fluke: duplicates are removed when moving from bank to bank",
      fun() ->
          {solution, _} = solve_puzzle(["fddccg", ""], Predators, []),
          {solution, _} = solve_puzzle("fddccg~")
      end},
     {"<facepalm> I thought this was unsolvable. The farmer outwitted me.",
      fun() ->
          GameInstructions = "fdcg~\nd eats c\nd eats g\n",
          {solution, _} = solve_puzzle(GameInstructions)
      end},
     {"Solvable with anything: t-rex doesn't exist (anymore) but defaults aren't generated",
      fun() ->
          TRex = $t,
          ExtinctPredator = [predator(TRex, "d,Dog"),
                             predator(TRex, "c,Chicken")],
          {solution, _} = solve_puzzle(["fdcg", ""], ExtinctPredator, [])
      end}].
