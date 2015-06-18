-record(chunk, {id   :: <<_:32>>,
                size :: pos_integer(),
                data :: binary()}).

-record(function, {name_ix :: pos_integer(),
                   arity   :: arity(),
                   label   :: pos_integer()}).
