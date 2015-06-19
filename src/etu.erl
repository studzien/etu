-module(etu).

-export([m/1,
         m/2]).

-include("etu.hrl").

-type export_spec() :: {Function :: atom(), arity()}. 
-type module_opt()  :: closures | sticky.
-type module_opts() :: [module_opt()].
-type module_ret()  :: {ok, Exported :: [export_spec()]} |
                       {error, sticky_directory} |
                       {error, code:load_error_rsn()}.

-type chunks() :: [#chunk{}].
-export_type([chunks/0,
              module_opts/0]).

%% API
-spec m(atom()) -> module_ret().
m(Module) ->
    m(Module, []).

-spec m(atom(), module_opts()) -> module_ret().
m(Module, Opts) ->
    case code:get_object_code(Module) of
        {Module, Code0, Filename} ->
            Beam = etu_beam:decode(Code0),
            Chunks = etu_ftab:migrate(Beam, decode_opts(Opts, [locals])),
            Code1 = etu_beam:encode(Chunks),
            maybe_load_code(Module, Filename, Code1, [], Opts);
        error ->
            {error, bad_module}
    end.

%% Internal
maybe_load_code(Module, Filename, Binary, Exported, Opts) ->
    case {code:is_sticky(Module), lists:member(sticky, Opts)} of
        {true, true} ->
            true = code:unstick_mod(Module),
            load_code(Module, Filename, Binary, Exported);
        {true, false} ->
            {error, sticky_directory};
        _ ->
            load_code(Module, Filename, Binary, Exported)
    end.

load_code(Module, Filename, Binary, Exported) ->
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} ->
            {ok, Exported};
        Error ->
            Error
    end.

decode_opts([], Acc) ->
    Acc;
decode_opts([Hd|Tl], Acc) ->
    case is_opt(Hd) of
        true  -> decode_opts(Tl, [Hd|Acc]);
        false -> decode_opts(Tl, Acc)
    end.

is_opt(closures) -> true;
is_opt(_)        -> false.
