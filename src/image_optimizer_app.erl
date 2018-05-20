% @hidden
-module(image_optimizer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  image_optimizer_sup:start_link().

stop(_State) ->
  ok.
