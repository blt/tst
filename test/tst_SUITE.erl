-module(tst_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("../include/test.hrl").

-export([all/0]).
-export([all_words_in_tree_are_members/1, tree_words_equal_to_input_length/1]).
-export([partial_matches_no_wildcards_is_member/1, partial_matches_no_wildcards_correct_length/1]).
-export([empty_spec_test/1, words_spec_test/1, create_spec_test/1, insert_spec_test/1]).
-export([partial_matches_spec_test/1, is_member_spec_test/1]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

all_words_in_tree_are_members(_Config) ->
    P = ?FORALL(Words, [string()],
                begin
                    TST = tst:create(Words),
                    lists:foldl(fun(W, Bool) ->
                                        Bool and tst:is_member(W, TST)
                                end, true, Words)
                end),
    true = proper:quickcheck(P, [long_result, verbose]).

tree_words_equal_to_input_length(_Config) ->
    P = ?FORALL(Words, [string()],
            begin
                Length = lists:foldl(fun(W, Sum) ->
                                             case length(W) of
                                                 0 -> Sum;
                                                 _ -> 1 + Sum
                                             end
                                     end, 0, Words),
                TST = tst:create(Words),
                Length =:= tst:words(TST)
            end),
    true = proper:quickcheck(P, [long_result, verbose]).

partial_matches_no_wildcards_is_member(_Config) ->
    P = ?FORALL(Words, [string()],
                begin
                    TST = tst:create(Words),
                    Word = rand_choose(Words),
                    lists:member(Word, tst:partial_matches(Word, TST))
                end),
    true = proper:quickcheck(P, [long_result, verbose]).

partial_matches_no_wildcards_correct_length(_Config) ->
    P = ?FORALL(Words, [string()],
                begin
                    TST = tst:create(Words),
                    Word = rand_choose(Words),
                    1 =:= length(tst:partial_matches(Word, TST))
                end),
    true = proper:quickcheck(P, [long_result, verbose]).


%% ====================================================
%% Spec Tests
%% ====================================================

empty_spec_test(_Config) ->
    true = proper:check_spec({tst, empty, 0}).

words_spec_test(_Config) ->
    true = proper:check_spec({tst, words, 1}).

create_spec_test(_Config) ->
    true = proper:check_spec({tst, create, 1}).

insert_spec_test(_Config) ->
    true = proper:check_spec({tst, insert, 2}).

partial_matches_spec_test(_Config) ->
    true = proper:check_spec({tst, partial_matches, 2}).

is_member_spec_test(_Config) ->
    true = proper:check_spec({tst, is_member, 2}).

%% ====================================================
%% Internal Functions
%% ====================================================

scrub([]) ->
    [];
scrub([$.|Rest]) ->
    Alpha = "abcdefghijklmnopqrstuvwxyz",
    Replacement = rand_choose(Alpha),
    [Replacement | scrub(Rest)];
scrub([C|Rest]) ->
    [C | scrub(Rest)].

-spec rand_choose(list(any())) -> any().
rand_choose(L) ->
    lists:nth(random:uniform(length(L)), L).
