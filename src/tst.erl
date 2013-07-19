-module(tst).
-export([
         contains/2,
         from_list/1,
         insert/2,
         partial_matches/2,
         near_neighbors/3,
         wc/1
        ]).

-record(nd, {
          data = ""   :: char(),
          lt = empty   :: tst(),
          eq = empty   :: tst(),
          gt = empty   :: tst(),
          is_word = false :: boolean()
         }).
-type tst() :: empty | #nd{}.

-export_type([tst/0]).

%% ===================================================================
%%  API
%% ===================================================================

-spec near_neighbors(Word::string(), TST::tst(), Distance::integer()) -> [string()].
%% @doc Rreturn a list of all words within Distance Hamming distance of Word.
%%
%% The Hamming distance of two strings is defined as the number of substitutions
%% needed to transform one into the other. "b" into "b" requires zero, "b" into
%% "a" one and "bee" into "fog" three, for example.
near_neighbors(Word, TST, Dist) ->
    lists:map(fun to_str/1, lists:flatten(near(to_ziplist(Word), TST, Dist))).

-spec partial_matches(Pattern::string(), TST::tst()) -> [string()].
%% @doc Return a list of partial match strings for the given Pattern.
%%
%% Partial match strings are those which have the same length as the Pattern but
%% may, with the inclusion of the wildcard '.' in the pattern-string, have
%% different characters in place of the wildcards. A tree containing strings
%% "word", "wold", "wood" will partially match all strings for pattern "w..d"
%% and the last solely for "w.od".
partial_matches(Pattern, TST) ->
    lists:map(fun to_str/1,
              lists:flatten(partial_matches(to_ziplist(Pattern), [], TST))).

-spec contains(string(), tst()) -> boolean().
%% @doc Return true if TST contains given word, else false.
%%
%% Please note that the tree is case sensitive. 'word' and 'Word' and 'WoRD' are
%% distinct words for this implementation.
contains(_, empty) ->
    false;
contains([], _) ->
    false;
contains([C], #nd{data=C, is_word=true}) ->
    true;
contains([C|Rest], #nd{data=C, eq=E}) ->
    contains(Rest, E);
contains([C|_Rest]=S, #nd{data=D, lt=L}) when C < D->
    contains(S, L);
contains([C|_Rest]=S, #nd{data=D, gt=R}) when C > D->
    contains(S, R).

-spec from_list([string()]) -> tst().
%% @doc Create a TST from a list of words.
%%
%% Please note that the tree is case sensitive. 'word' and 'Word' and 'WoRD' are
%% distinct words for this implementation.
from_list(Words) ->
    lists:foldl(fun insert/2, empty, Words).

-spec insert(string(), tst()) -> tst().
%% @doc Insert a single word into a TST.
%%
%% Please note that the tree is case sensitive. 'word' and 'Word' and 'WoRD' are
%% distinct words for this implementation.
insert([], TST) ->
    TST;
insert([C], empty) ->
    #nd{data=C, is_word=true};
insert([C|Rest], empty) ->
    #nd{data=C, eq=insert(Rest, empty)};
insert([C], #nd{data=C}=N) ->
    N;
insert([C|Rest], #nd{data=C, eq=E}=N) ->
    N#nd{eq=insert(Rest, E)};
insert([C|_Rest]=S, #nd{data=D, lt=L}=N) when C < D ->
    N#nd{lt=insert(S, L)};
insert([C|_Rest]=S, #nd{data=D, gt=R}=N) when C > D ->
    N#nd{gt=insert(S, R)}.

-spec wc(tst()) -> non_neg_integer().
%% @doc Count the number of words in the tree.
wc(empty)                                -> 0;
wc(#nd{lt=L, eq=E, gt=G, is_word=false}) -> 0 + wc(L) + wc(E) + wc(G);
wc(#nd{lt=L, eq=E, gt=G, is_word=true})  -> 1 + wc(L) + wc(E) + wc(G).

%% ===================================================================
%%  Internal Functions
%% ===================================================================

-type ziplist() :: empty | {Left::string(), Cur::char(), Right::string()}.

-spec to_ziplist(list()) -> ziplist().
to_ziplist([]) ->
    empty;
to_ziplist([C|Rest]) ->
    {[], C, Rest}.

-spec zipfwd(ziplist()) -> ziplist() | end_of_string.
zipfwd({_Left, _Cur, []}) ->
    end_of_string;
zipfwd({Left, Cur, [C|Rest]}) ->
    {[Cur|Left], C, Rest}.

-spec ziprepl(ziplist(), char()) -> ziplist().
ziprepl({Left, _Cur, Right}, Char) ->
    {Left, Char, Right}.

-spec to_str(ziplist()) -> string().
to_str(empty) ->
    "";
to_str({[C|Rest], Cur, Right}) ->
    to_str({Rest, C, [Cur|Right]});
to_str({[], Cur, Right}) ->
    [Cur | Right].

-spec partial_matches(Zip::ziplist(), Acc::[ziplist()], tst()) -> [ziplist()].
partial_matches(end_of_string, _, _TST) ->
    [];
partial_matches(_, _, empty) ->
    [];
partial_matches({_L, $., []}=Zip, WordAcc, N=#nd{data=C, is_word=true}) ->
    [partial_matches(Zip, [], N#nd.lt),
     partial_matches(Zip, [], N#nd.gt),
     ziprepl(Zip, C) | WordAcc];
partial_matches({_L, $., []}, WordAcc, #nd{data=_C, is_word=false}) ->
    WordAcc;
partial_matches({_L, C, []}=Zip, WordAcc, #nd{data=C, is_word=true}) ->
     [Zip | WordAcc];
partial_matches({_L, $., _R}=Zip, WordAcc, N=#nd{data=C, is_word=_B}) ->
    [partial_matches(Zip, [], N#nd.lt),
     partial_matches(zipfwd(ziprepl(Zip, C)), [], N#nd.eq),
     partial_matches(Zip, [], N#nd.gt) | WordAcc];
partial_matches({_L, C, _R}=Zip, WordAcc, N=#nd{data=D, is_word=_B}) when C < D ->
    partial_matches(Zip, WordAcc, N#nd.lt);
partial_matches({_L, C, _R}=Zip, WordAcc, N=#nd{data=D, is_word=_B}) when C > D ->
    partial_matches(Zip, WordAcc, N#nd.gt);
partial_matches({_L, C, _R}=Zip, WordAcc, N=#nd{data=C, is_word=_B}) ->
    partial_matches(zipfwd(Zip), WordAcc, N#nd.eq).

-spec near(Word::ziplist(), TST::tst(), Distance::integer()) -> [ziplist()].
near(end_of_string, _TST, _Dist) ->
    [];
near(_Word, empty, _Dist) ->
    [];
near(_Word, _TST, Dist) when Dist < 0 ->
    [];

near({_L, C, []}=Word, #nd{data=C, is_word=true}, _Dist) ->
    [Word];
near({_L, C, []}=Word, #nd{data=D, is_word=true}=TST, Dist) when C =/= D ->
    [
     near(Word, TST#nd.lt, Dist),
     ziprepl(Word, D),
     near(Word, TST#nd.gt, Dist)
    ];
near({_L, C, _R}=Word, #nd{data=D, is_word=_Bool}=TST, Dist) ->
    {EqWord, EqDist} = case C =:= D of
                           true  -> {zipfwd(Word), Dist};
                           false -> {zipfwd(ziprepl(Word, D)), Dist-1}
                       end,

    [
     near(Word,   TST#nd.lt, Dist),
     near(EqWord, TST#nd.eq, EqDist),
     near(Word,   TST#nd.gt, Dist)
    ].

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

near_neighbors_test_() ->
    Words    = ["b", "berries", "bat", "aat", "cat", "bet", "pet", "parries", "z"],

    TST = from_list(Words),

    [
     ?_assertMatch([], near_neighbors("b", empty, 2)),
     ?_assertMatch([], near_neighbors("b", TST, -1)),

     { "identy searches",
       [
        ?_assertMatch(["bat"], near_neighbors("bat", TST, 0)),
        ?_assertMatch(["aat"], near_neighbors("aat", TST, 0))
       ]
     },

     { "near searches",
       [
        ?_assertMatch(["aat", "bat", "bet", "cat"],
                      near_neighbors("bat", TST, 1)),
        ?_assertMatch(["aat", "bat", "bet", "cat", "pet"],
                      near_neighbors("bat", TST, 2)),
        ?_assertMatch(["berries"],
                      near_neighbors("berries", TST, 1)),
        ?_assertMatch(["berries", "parries"],
                      near_neighbors("berries", TST, 2)),
        ?_assertMatch(["b", "z"],
                      near_neighbors("z", TST, 1)),
        ?_assertMatch([],
                      near_neighbors("not_in_tree", TST, 2))
       ]
     }
    ].

partial_matches_test_() ->
    Words = ["b", "a", "ab", "bb", "bat", "rax", "bath", "bad", "daz", "bed", ""],
    TST = from_list(Words),

    [
     { "explicit partial matches",
       [
        ?_assertMatch([],       partial_matches("bat", empty)),
        ?_assertMatch(["b"],    partial_matches("b", TST)),
        ?_assertMatch(["a"],    partial_matches("a", TST)),
        ?_assertMatch(["ab"],   partial_matches("ab", TST)),
        ?_assertMatch(["bat"],  partial_matches("bat", TST)),
        ?_assertMatch(["rax"],  partial_matches("rax", TST)),
        ?_assertMatch(["bath"], partial_matches("bath", TST))
       ]
     },
     { "wildcard matches",
       [
        ?_assertMatch(["a", "b"],     partial_matches(".", TST)),
        ?_assertMatch(["ab"],         partial_matches("a.", TST)),
        ?_assertMatch(["ab", "bb"],   partial_matches(".b", TST)),
        ?_assertMatch(["bad", "bat"], partial_matches("ba.", TST)),
        ?_assertMatch(["bad", "bed"], partial_matches("b.d", TST)),
        ?_assertMatch(["bath"],       partial_matches("ba..", TST)),
        ?_assertMatch(["bad", "bat", "daz", "rax"],
                      partial_matches(".a.", TST)),
        ?_assertMatch(["bad", "bat", "bed", "daz", "rax"],
                      partial_matches("...", TST))
       ]
     }
    ].

ziplist_test_() ->
    [
     { "string conversion",
       [
        ?_assertMatch("",    to_str(empty)),
        ?_assertMatch("bat", to_str({[], $b, "at"})),
        ?_assertMatch("bat", to_str({[$b], $a, "t"})),
        ?_assertMatch("bat", to_str({[$a, $b], $t, ""}))
       ]
     },
     { "character replacing",
       [
        ?_assertMatch({[$b, $a], $c, []}, ziprepl({[$b, $a], $d, []}, $c))
       ]
     },
     { "forward movement",
       [
        ?_assertMatch(end_of_string,      zipfwd({[$a, $b], $t, ""})),
        ?_assertMatch({[$a, $b], $t, ""}, zipfwd({[$b], $a, "t"})),
        ?_assertMatch({[$b], $a, "t"},    zipfwd({[], $b, "at"}))
       ]
     },
     { "creation",
       [
        ?_assertMatch(empty,          to_ziplist("")),
        ?_assertMatch({[], $b, "at"}, to_ziplist("bat"))
       ]
     }
    ].

contains_test_() ->
    Words = ["bat", "batman", "bath", "rabbit"],
    TST = from_list(Words),

    [
     ?_assertMatch(false, contains("bat", empty)),

     ?_assertMatch(true,  contains("bat", TST)),
     ?_assertMatch(true,  contains("batman", TST)),
     ?_assertMatch(true,  contains("bath", TST)),
     ?_assertMatch(true,  contains("rabbit", TST)),

     ?_assertMatch(false, contains("ba", TST))
    ].

insertion_test_() ->
    Ten       = #nd{data=10, is_word=true},
    TenTwenty = #nd{data=10, eq=#nd{data=20, is_word=true}},
    TenTwenty_Five = #nd{data=10,
                         lt=#nd{data=5,  is_word=true},
                         eq=#nd{data=20, is_word=true}},
    TenTwenty_FiveFour = #nd{data=10,
                             lt=#nd{data=5,
                                    eq=#nd{data=4, is_word=true}},
                             eq=#nd{data=20, is_word=true}},
    TenTwenty_Thirty = #nd{data=10,
                          eq=#nd{data=20, is_word=true},
                          gt=#nd{data=30, is_word=true}
                         },
    TenTwenty_TenTen = #nd{data=10,
                           eq=#nd{data=20,
                                  lt=#nd{data=10, is_word=true},
                                  is_word=true}
                          },

    [
     { "word by word inserts",
       [
        ?_assertMatch(empty,              insert([], empty)),
        ?_assertMatch(Ten,                insert([], Ten)),
        ?_assertMatch(Ten,                insert([10], empty)),
        ?_assertMatch(TenTwenty,          insert([10, 20], empty)),
        ?_assertMatch(TenTwenty,          insert([10, 20], TenTwenty)),
        ?_assertMatch(TenTwenty_Five,     insert([5], TenTwenty)),
        ?_assertMatch(TenTwenty_FiveFour, insert([5, 4], TenTwenty)),
        ?_assertMatch(TenTwenty_Thirty,   insert([30], TenTwenty)),
        ?_assertMatch(TenTwenty_TenTen,   insert([10,10], TenTwenty))
       ]
     },
     { "bulk word inserts",
       [
        ?_assertMatch(TenTwenty_TenTen,   from_list([[10,20], [10,10]])),
        ?_assertMatch(TenTwenty_Thirty,   from_list([[10,20], [30]])),
        ?_assertMatch(TenTwenty_FiveFour, from_list([[10,20], [5,4]]))
       ]
     }
    ].

word_count_test_() ->
    [
     ?_assertMatch(0, wc(from_list([]))),
     ?_assertMatch(1, wc(from_list(["brian"]))),
     ?_assertMatch(2, wc(from_list(["brian", "briAN"]))),
     ?_assertMatch(5, wc(from_list(["batman", "doesn't", "afraid", "of", "anything"])))
    ].

shuffle(L) ->
    [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- L])].

time(Mod, Fun, Args) ->
    {Time, Value} = timer:tc(Mod, Fun, Args),
    ?debugFmt("~p:~p/~p :: ~.3f seconds", [Mod, Fun, length(Args),
                                           Time*0.000001]),
    Value.

benchmark_test_() ->
    ?debugFmt("~n%% ================================================================~n"
              "%%  Benchmarks~n"
              "%% ================================================================~n",
              []),

    {ok, Dictionary} = file:read_file("/usr/share/dict/words"),
    Words = shuffle(lists:map(fun binary_to_list/1,
                                   binary:split(Dictionary, <<"\n">>, [global]))),
    [_, _, _, _, _, _, _, _, _, _, Word | _] = Words,

    {timeout, 60, [
                   fun() ->
                           DictTST = time(tst, from_list, [Words]),
                           TST = time(tst, insert, ["TheRainInSpain", DictTST]),

                           true = time(tst, contains, [Word, TST]),

                           _ = time(tst, partial_matches, [".e.d", TST]),

                           _ = time(tst, near_neighbors, [Word, TST, 0]),
                           _ = time(tst, near_neighbors, [Word, TST, 1]),
                           _ = time(tst, near_neighbors, [Word, TST, 2]),
                           _ = time(tst, near_neighbors, [Word, TST, 3]),
                           _ = time(tst, near_neighbors, [Word, TST, 4]),
                           _ = time(tst, near_neighbors, [Word, TST, 5]),
                           _ = time(tst, near_neighbors, [Word, TST, 6]),
                           _ = time(tst, near_neighbors, [Word, TST, 7])
                   end
                  ]
    }.

-endif.
