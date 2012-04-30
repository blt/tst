-module(tst).
-export([empty/0, words/1, create/1, insert/2, is_member/2, partial_matches/2]).
-export_type([tst/0]).

-record(tst_node, { nodeChar=""  :: char(),
                    ltTST=empty  :: tst(),
                    eqTST=empty  :: tst(),
                    gtTST=empty  :: tst()
                  }).
-type tst() :: empty | #tst_node{}.

-spec empty() -> tst().
empty() -> empty.

-spec partial_matches(string(), tst()) -> [string()].
partial_matches(Str, TST) ->
    lists:map(fun(E) -> {word, W} = E, lists:reverse(W) end,
              lists:flatten(partial_matches(Str, TST, []))).

-spec partial_matches(string(), tst(), string()) -> [{word,nonempty_string()}].
partial_matches([], empty, Accum) ->
    [{word, Accum}];
partial_matches([], #tst_node{}, _Accum) ->
    [];
partial_matches(_Word, empty, _Accum) ->
    [];
partial_matches(Word=[$.|Rest], TST=#tst_node{nodeChar=C}, Accum) ->
    Less = partial_matches(Word, TST#tst_node.ltTST, Accum),
    Equal = partial_matches(Rest, TST#tst_node.eqTST, [C|Accum]),
    Greater = partial_matches(Word, TST#tst_node.gtTST, Accum),
    lists:filter((fun not_empty/1), [Less, Equal, Greater]);
partial_matches(Word=[C|Rest], TST=#tst_node{nodeChar=NC}, Accum) ->
    case compare(C, NC) of
        lt ->
            partial_matches(Word, TST#tst_node.ltTST, Accum);
        eq ->
            partial_matches(Rest, TST#tst_node.eqTST, [C|Accum]);
        gt ->
            partial_matches(Word, TST#tst_node.gtTST, Accum)
    end.

-spec is_member(string(), tst()) -> boolean().
is_member([], empty) ->
    true;
is_member(_Word, empty) ->
    false;
is_member([], #tst_node{}) ->
    false;
is_member(Word=[C|Rest], TST=#tst_node{nodeChar=NC}) ->
    case compare(NC, C) of
        lt ->
            is_member(Word, TST#tst_node.ltTST);
        eq ->
            is_member(Rest, TST#tst_node.eqTST);
        gt ->
            is_member(Word, TST#tst_node.gtTST)
    end.

-spec words(tst()) -> integer().
words(empty) ->
    0;
words(#tst_node{eqTST=empty}) ->
    1;
words(#tst_node{ltTST=L, eqTST=E, gtTST=G}) ->
    words(L) + words(E) + words(G).

-spec create([string()]) -> tst().
create(Words) ->
    create(Words, empty).

-spec insert(string(), tst()) -> tst().
insert([], TST) ->
    TST;
insert([C], empty) ->
    #tst_node{nodeChar=C};
insert([C|Rest], empty) ->
    #tst_node{nodeChar=C, eqTST=insert(Rest, empty)};
insert(Word, TST) ->
    [C|Rest] = Word,
    case compare(C, TST#tst_node.nodeChar) of
        lt ->
            TST#tst_node{ltTST=insert(Word, TST#tst_node.ltTST)};
        eq ->
            TST#tst_node{eqTST=insert(Rest, TST#tst_node.eqTST)};
        gt ->
            TST#tst_node{gtTST=insert(Word, TST#tst_node.gtTST)}
    end.

%% ====================================================
%% Internal Functions
%% ====================================================

-spec create([string()], tst()) -> tst().
create([], Tst) ->
    Tst;
create([Word|Rest], Tst) ->
    NewTst = insert(Word, Tst),
    create(Rest, NewTst).

-type order() :: lt | eq | gt.
-spec compare(any(), any()) -> order().
compare(A, B) when A  <  B -> lt;
compare(A, B) when A =:= B -> eq;
compare(A, B) when A  >  B -> gt.

-spec is_empty(list()) -> boolean().
is_empty(L) ->
    case L of
        [] ->
            true;
        _ ->
            false
    end.

-spec not_empty(list()) -> boolean().
not_empty(L) ->
    not is_empty(L).
