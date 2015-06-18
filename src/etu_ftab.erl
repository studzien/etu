-module(etu_ftab).

-include("etu.hrl").

-export([migrate/2]).

%% API
-spec migrate(etu:chunks(), etu:module_opts()) -> etu:chunks().
migrate(Chunks0, Opts) ->
    LocalTab1 = chunk_to_tab(<<"LocT">>, Chunks0),
    ExportTab1 = chunk_to_tab(<<"ExpT">>, Chunks0),
    {Migrate, LocalTab2} = candidates(LocalTab1, Opts),
    ExportTab2 = merge(ExportTab1, Migrate),
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

candidates(LocalTab, _Opts) ->
    {LocalTab, []}.
