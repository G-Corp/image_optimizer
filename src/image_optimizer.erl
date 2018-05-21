-module(image_optimizer).

-export([
         optimize/1
         , optimize/2

         % hidden
         , run/1
         , os_find_executable/1
        ]).

-type options() :: [option()].
-type option() :: {quality, integer()}
                  | {quiet, true | false}
                  | {strip_metadata, true | false}
                  | {output, file:filename_all()}.

-callback optimize(Executable :: file:filename_all(), File :: file:filename_all(), Options :: options()) ->
  {ok, Output :: file:filename_all()} | {error, Reason :: term()}.

% @equiv optimize(File, [])
optimize(File) ->
  optimize(File, []).

% @doc
% Optimize an image file
% @end
-spec optimize(File :: file:filename_all(), Options :: options()) -> {ok, file:filename_all()} | {error, term()} | {error, integer(), string()}.
optimize(File, Options) ->
  case filelib:is_regular(bucs:to_string(File)) of
    true ->
      case optimizer(bucs:to_string(File), Options) of
        undefined ->
          {error, invalid_file};
        Optimizers ->
          optimize(Optimizers, File, Options, {error, optimizer_not_found})
      end;
    false ->
      {error, file_not_found}
  end.

% @hidden
run(Cmd) ->
  bucos:run(Cmd, [{timeout, infinity}, stdout_on_error, display_stdout, {return, combined, all}]).

optimize([], _File, _Options, Error) -> Error;
optimize([Optimizer|Rest], File, Options, Error) ->
  Module = bucs:to_atom(lists:flatten(io_lib:format("image_optimizer_~s", [Optimizer]))),
  case find_executable(Optimizer) of
    false ->
      optimize(Rest, File, Options, Error);
    AppPath ->
      case bucs:apply(Module, optimize, [AppPath, File, Options], {error, optimizer_not_found}) of
        {error, optimizer_not_found} ->
          optimize(Rest, File, Options, Error);
        NewError when element(1, NewError) == error ->
          optimize(Rest, File, Options, NewError);
        {ok, _Filename} = Ok ->
          Ok
      end
  end.

optimizer("png") -> [pngquant, optipng];
optimizer("jpeg") -> [jpegoptim, cjpeg];
optimizer("jpg") -> [jpegoptim, cjpeg];
optimizer("svg") -> [svgo, scour];
optimizer("gif") -> [gifsicle];
optimizer(_) -> undefined.

optimizer(File, Options) ->
  case proplists:get_bool(identify, Options) of
    true ->
      case find_executable(identify) of
        false ->
          optimizer_by_extension(File);
        Identify ->
          case bucos:run({"~ts -format \"%m\" \"~ts\"", [Identify, File]}) of
            {ok, Format} ->
              optimizer(bucstring:lowercase(Format));
            _ ->
              undefined
          end
      end;
    false ->
      optimizer_by_extension(File)
  end.

optimizer_by_extension(File) ->
  case filename:extension(File) of
    [$.|Extension] ->
      optimizer(bucstring:lowercase(Extension));
    _ ->
      undefined
  end.

find_executable(Exec) when is_atom(Exec) ->
  case doteki:get_env(Exec, undefined) of
    undefined ->
      case image_optimizer:os_find_executable(bucs:to_string(Exec)) of
        false ->
          false;
        AppPath ->
          AppPath
      end;
    AppPath ->
      case bucfile:is_executable(AppPath) of
        #{owner := false,
          group := false,
          other := false} ->
          false;
        _ ->
          bucs:to_string(AppPath)
      end
  end.

% @hidden
% Needed for tests
os_find_executable(Exec) ->
  os:find_executable(Exec).
