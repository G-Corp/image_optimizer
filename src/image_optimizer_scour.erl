% @hidden
-module(image_optimizer_scour).
-behaviour(image_optimizer).

-export([optimize/3]).

optimize(Executable, File, Options) ->
  Cmd = [Executable,
         options(Options, [{quiet, true}]),
         <<" -o ", (output(File))/binary>>,
         <<" -i ", (bucs:to_binary(File))/binary>>],
  case image_optimizer:run(iolist_to_binary(Cmd)) of
    {ok, _} ->
      copy_output(output(File), File, Options);
    Error ->
      file:delete(output(File)),
      Error
  end.


output(File) ->
  iolist_to_binary(
    string:replace(bucs:to_string(File), filename:extension(bucs:to_string(File)), "-io.svg", trailing)
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
  options(Options, Default, []).

options(_Options, [], Acc) ->
  lists:reverse(Acc);
options(Options, [{Option, Default}|Rest], Acc) ->
  Value = proplists:get_value(Option, Options, Default),
  options(
    Options,
    Rest,
    format(Option, Value, Acc)).

format(quiet, true, Acc) ->
  [" --quiet"|Acc];
format(_Option, _Value, Acc) ->
  Acc.
