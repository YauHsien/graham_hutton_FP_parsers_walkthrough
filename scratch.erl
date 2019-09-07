-module(scratch).
-compile(export_all).
-export([]).

%% Primitive parsers

succeed(V) ->
    fun(In) -> [{V,In}] end.

fail(_In) ->
    [].

satisfy(P) ->
    fun([]) ->
            fail([]);
       ([X|Xs]) ->
            case P(X) of
                true -> (succeed(X))(Xs);
                false -> fail(Xs)
            end
    end.

%% Combinators

alt(P1, P2) ->
    fun(In) -> erlang:append(P1(In), P2(In)) end.

then(P1, P2) ->
    fun(In) -> [ {{V1,V2},Out2} || {V1,Out1} <- P1(In), {V2,Out2} <- P2(Out1) ] end.

using(P, F) ->
    fun(In) -> [ {F(V),Out} || {V,Out} <- P(In) ] end.

cons({X,Xs}) -> [X|Xs].

many(P) ->
    fun(In) ->
            (alt(using(then(P, many(P)),fun cons/1), succeed([])))(In)
    end.

some(P) ->
    using(then(P,many(P)), fun cons/1).

snd({_,X}) ->
    X.

fst({X,_}) ->
    X.

xthen(P1, P2) ->
    using(then(P1,P2), fun snd/1).

thenx(P1, P2) ->
    using(then(P1,P2), fun fst/1).

const(V) ->
    fun(_In) -> V end.

return(P, V) ->
    using(P, const(V)).

%% Applications

literal(X) ->
    satisfy(fun(Y) -> X==Y end).

digit(X) when $0 =< X andalso X =< $9 ->
    true;
digit(_) ->
    false.

number() ->
    some(satisfy(fun digit/1)).

letter(X) when ($a =< X andalso X =< $z) orelse ($A =< X andalso X =< $Z) ->
    true;
letter(_) ->
    false.

word() ->
    some(satisfy(fun letter/1)).

string([]) ->
    succeed([]);
string([X|Xs]) ->
    using(then(literal(X),string(Xs)), fun cons/1).

plus({X,Y}) ->
    {add, X, Y}.

minus({X,Y}) ->
    {sub, X, Y}.

times({X,Y}) ->
    {mul, X, Y}.

divide({X,Y}) ->
    {'div', X, Y}.

value(X) ->
    X.

%% Expressions

expn(In) ->
    Term = fun term/1,
    P1 = using(then(Term,xthen(literal($+),Term)), fun plus/1),
    P2 = using(then(Term,xthen(literal($-),Term)), fun minus/1),
    (alt(alt(P1,P2), Term))(In).

term(In) ->
    Factor = fun factor/1,
    P1 = using(then(Factor,xthen(literal($*),Factor)), fun times/1),
    P2 = using(then(Factor,xthen(literal($/),Factor)), fun divide/1),
    (alt(alt(P1,P2), Factor))(In).

factor(In) ->
    Expn = fun expn/1,
    P1 = using(number(), fun value/1),
    P2 = xthen(literal($(), thenx(Expn,literal($)))),
    (alt(P1, P2))(In).

%% Layout

any(G) ->
    fun(As) -> lists:foldl(fun(E,A) -> alt(G(E),A) end, fun fail/1, As) end.

white(In) ->
    (many((any(fun literal/1))(" \t\n")))(In).

nibble(P) ->
    xthen(fun white/1, thenx(P, fun white/1)).

symbol(In) ->
    nibble(string(In)).
