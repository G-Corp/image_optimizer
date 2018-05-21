% @hidden
-module(image_optimizer_pngquant).
-behaviour(image_optimizer).

-export([optimize/3]).

-define(EXT, "-io.png").

optimize(Executable, File, Options) ->
  Cmd = [Executable,
         options(Options, [{quality, 80}]),
         <<" -- ", (bucs:to_binary(File))/binary>>],
  case image_optimizer:run(iolist_to_binary(Cmd)) of
    {ok, _} ->
      copy_output(output(File), File, Options);
    Error ->
      file:delete(output(File)),
      Error
  end.


output(File) ->
  bucs:to_string(
    iolist_to_binary(
      string:replace(bucs:to_string(File), filename:extension(bucs:to_string(File)), "-io.png", trailing)
     )
   ).

copy_output(Output, File, Options) ->
  NewOutput = proplists:get_value(output, Options, File),
  case file:copy(Output, NewOutput) of
    {ok, _BytesCopied} ->
      file:delete(Output),
      {ok, NewOutput};
    {error, _Reason} = Error ->
      file:delete(Output),
      Error
  end.

options(Options, Default) ->
  options(Options, Default, [" --skip-if-larger", " --speed 1", " --force", " --ext -io.png"]).

options(_Options, [], Acc) ->
  lists:reverse(Acc);
options(Options, [{Option, Default}|Rest], Acc) ->
  Value = proplists:get_value(Option, Options, Default),
  options(
    Options,
    Rest,
    format(Option, Value, Acc)).

format(quality, Value, Acc) ->
  [io_lib:format(" --quality=~p", [Value])|Acc];
format(_Option, _Value, Acc) ->
  Acc.
