% @hidden
-module(image_optimizer_optipng).
-behaviour(image_optimizer).

-export([optimize/3]).

optimize(Executable, File, Options) ->
  case output(File, Options) of
    {ok, OutputFile} ->
      Cmd = [Executable,
             options(Options, [{quality, 30}, {quiet, true}, {strip_metadata, false}]),
             <<" ", (bucs:to_binary(OutputFile))/binary>>],
      case image_optimizer:run(iolist_to_binary(Cmd)) of
        {ok, _} ->
          {ok, OutputFile};
        Error ->
          Error
      end;
    Error ->
      Error
  end.

output(File, Options) ->
  case proplists:get_value(output, Options, File) of
    File ->
      {ok, File};
    OutputFile ->
      case file:copy(File, OutputFile) of
        {ok, _BytesCopied} ->
          {ok, OutputFile};
        {error, _Reason} = Error ->
          Error
      end
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

format(quality, Value, Acc) ->
  [io_lib:format(" -o~p", [bucs:to_integer((100-Value)/10)])|Acc];
format(quiet, true, Acc) ->
  [" -quiet"|Acc];
format(strip_metadata, true, Acc) ->
  [" -strip all"|Acc];
format(_Option, _Value, Acc) ->
  Acc.
