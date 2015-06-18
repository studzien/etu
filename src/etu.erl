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
            Chunks1 = etu_beam:decode(Code0),
            Chunks2 = etu_ftab:migrate(Chunks1, decode_opts(Opts, [all])),
            Code1 = etu_beam:encode(Chunks2),
            maybe_load_code(Module, Filename, Code1, [], Opts);
        error ->
            {error, bad_module}
    end.

%% Internal
decode_opts([], Acc) ->
    Acc;
decode_opts([closures|Tl], Acc) ->
    decode_opts(Tl, [closures|Acc]);
decode_opts([_|Tl], Acc) ->
    decode_opts(Tl, Acc).

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
