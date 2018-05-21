% @hidden
-module(image_optimizer_cjpeg).
-behaviour(image_optimizer).

-export([optimize/3]).

optimize(Executable, File, Options) ->
  case output(File, Options) of
    {ok, OutputFile} ->
      Cmd = [Executable,
             options(Options, [{quality, 80}, {quiet, true}]),
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
  options(Options, Default, [" -optimize", " -targa"]).

options(_Options, [], Acc) ->
  lists:reverse(Acc);
options(Options, [{Option, Default}|Rest], Acc) ->
  Value = proplists:get_value(Option, Options, Default),
  options(
    Options,
    Rest,
    format(Option, Value, Acc)).

format(quality, Value, Acc) ->
  [io_lib:format(" -quality ~p", [Value])|Acc];
format(quiet, false, Acc) ->
  [" -verbose"|Acc];
format(_Option, _Value, Acc) ->
  Acc.
