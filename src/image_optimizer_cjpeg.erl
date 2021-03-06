% @hidden
-module(image_optimizer_cjpeg).
-behaviour(image_optimizer).

-export([optimize/3]).

optimize(Executable, File, Options) ->
  {Rename, Input, Output} = output(File, Options),
  Cmd = [Executable,
         options(Options, [{quality, 75}, {quiet, true}]),
         <<" ", (bucs:to_binary(Input))/binary, " >", (bucs:to_binary(Output))/binary>>],
  case image_optimizer:run(iolist_to_binary(Cmd)) of
    {ok, _} ->
      case Rename of
        true ->
          case file:copy(Output, Input) of
            {ok, _BytesCopied} ->
              {ok, Input};
            {error, _Reason} = Error ->
              Error
          end;
        _ ->
          {ok, Output}
      end;
    Error ->
      Error
  end.

output(File, Options) ->
  case proplists:get_value(output, Options, File) of
    File ->
      {true, File, tempfile:name("prefix_", [{ext, extension(File)}])};
    OutputFile ->
      {false, File, OutputFile}
  end.

extension(File) ->
  case bucs:to_binary(filename:extension(File)) of
    <<".", Ext>> -> Ext;
    _ -> "tmp"
  end.

options(Options, Default) ->
  options(Options, Default, [" -optimize"]).

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
