-module(image_optimizer).

-export([optimize/2]).

-type options() :: [option()].
-type option() :: {level, integer()}
                  | {quality, integer()}
                  | {quiet, true | false}
                  | {strip_metadata, true | false}
                  | {identify, true | false}
                  | {output, file:filename_all()}.

% @doc
% @end
-spec optimize(File :: file:filename_all(), Options :: options()) -> ok | {error, term()}.
optimize(File, Options) ->
  todo.
