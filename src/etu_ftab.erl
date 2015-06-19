-module(etu_ftab).

-include("etu.hrl").

-export([migrate/2]).

%% API
-spec migrate({tuple(), etu:chunks()}, etu:module_opts()) -> etu:chunks().
migrate({Atoms, Chunks0}, Opts) ->
    LocalTab1 = chunk_to_tab(<<"LocT">>, Chunks0),
    ExportTab1 = chunk_to_tab(<<"ExpT">>, Chunks0),
    {Migrated, LocalTab2} = candidates(LocalTab1, Atoms, Opts),
    ExportTab2 = merge(ExportTab1, Migrated),
    Chunks1 = add_to_chunks(LocalTab2, <<"LocT">>, Chunks0),
    add_to_chunks(ExportTab2, <<"ExpT">>, Chunks1).

%% Internal
chunk_to_tab(ChunkName, Chunks) ->
    #chunk{data = Data} = lists:keyfind(ChunkName, #chunk.id, Chunks),
    decode_tab(Data).

add_to_chunks(Tab, ChunkName, Chunks) ->
    Data = encode_tab(Tab),
    Chunk = #chunk{id = ChunkName, size = byte_size(Data), data = Data},
    lists:keyreplace(ChunkName, #chunk.id, Chunks, Chunk).

encode_tab(Tab) ->
    Functions = << <<NameIx:32, Arity:32, Label:32>>
                  || #function{name_ix = NameIx,
                               arity = Arity,
                               label = Label} <- Tab >>,
    Count = length(Tab),
    << Count:32, Functions/binary >>.

decode_tab(<<Count:32, Data/binary>>) ->
    decode_tab(Count, Data, []).

decode_tab(0, _, Acc) ->
    lists:reverse(Acc);
decode_tab(N, <<NameIx:32, Arity:32, Label:32, Rest/binary>>, Acc) ->
    Function = #function{name_ix = NameIx, arity = Arity, label = Label},
    decode_tab(N-1, Rest, [Function|Acc]).

merge(X, Y) ->
    lists:keysort(#function.name_ix, X ++ Y).

candidates(LocalTab, Atoms, Opts) ->
    lists:foldl(fun(Opt, {Migrated0, Left0}) ->
                F = candidates_partition_fun(Opt, Atoms),
                {Migrated1, Left1} = lists:partition(F, Left0),
                {Migrated1 ++ Migrated0, Left1}
        end, {[], LocalTab}, Opts).

candidates_partition_fun(locals, Atoms) ->
    fun(F) -> not is_closure(F, Atoms) end;
candidates_partition_fun(closures, Atoms) ->
    fun(F) -> is_closure(F, Atoms) end.

is_closure(#function{name_ix = Index}, Atoms) ->
    Name = element(Index, Atoms),
    case re:run(Name, <<"^-.+/\\d+-fun-\\d+-$">>) of
        {match, _} -> true;
        nomatch    -> false
    end.
