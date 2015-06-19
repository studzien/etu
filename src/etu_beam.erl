-module(etu_beam).

-include("etu.hrl").

-export([decode/1,
         encode/1]).

%% API
-spec decode(<<_:64, _:_*8>>) -> {tuple(), etu:chunks()}.
decode(Code) ->
    <<"FOR1", _Size:32, "BEAM", ChunksData/binary>> = Code,
    Chunks = decode(ChunksData, []),
    {atom_table(Chunks), Chunks}.

-spec encode(etu:chunks()) -> binary().
encode(Chunks) ->
    ChunksData = << <<Id/binary, Size:32, (pad(Data))/binary>>
                   || #chunk{id = Id, size = Size, data = Data} <- Chunks >>,
    Size = byte_size(ChunksData) + 4,
    << "FOR1", Size:32, "BEAM", ChunksData/binary >>.

%% Internal
decode(<<>>, Acc) ->
    lists:reverse(Acc);
decode(<<Id:32, Size:32, Rest0/binary>>, Acc) ->
    PaddedSize = chunk_size(Size),
    {Data, Rest1} = erlang:split_binary(Rest0, PaddedSize),
    Chunk = #chunk{id = <<Id:32>>, size = Size, data = Data},
    decode(Rest1, [Chunk|Acc]).

atom_table(Chunks) ->
    #chunk{data = Data} = lists:keyfind(<<"Atom">>, #chunk.id, Chunks),
    << Count:32, AtomData/binary >> = Data,
    atom_table(AtomData, Count, []).

atom_table(_, 0, Acc) ->
    list_to_tuple(lists:reverse(Acc));
atom_table(<< Length:8, Rest0/binary >>, N, Acc) ->
    {Atom, Rest1} = erlang:split_binary(Rest0, Length),
    atom_table(Rest1, N-1, [Atom|Acc]).

chunk_size(Size) ->
    case Size rem 4 of
        0 -> Size;
        N -> Size + (4 - N)
    end.

pad(Chunk) ->
    Pad = case byte_size(Chunk) rem 4 of
        0 -> <<>>;
        N -> binary:copy(<<0>>, 4-N)
    end,
    << Chunk/binary, Pad/binary >>.
