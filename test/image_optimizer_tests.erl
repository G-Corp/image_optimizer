-module(image_optimizer_tests).
-include_lib("eunit/include/eunit.hrl").

image_optimizer_test_() ->
  {setup,
   fun() ->
       meck:new(filelib, [passthrough, unstick]),
       meck:expect(filelib, is_regular, fun(_) -> true end),
       meck:new(file, [passthrough, unstick]),
       meck:expect(file, copy, fun(_, _) -> {ok, 0} end),
       meck:expect(file, delete, fun(_) -> ok end),
       meck:new(image_optimizer, [passthrough]),
       meck:expect(image_optimizer, os_find_executable, fun
                                                          ("identify") -> false;
                                                          (Other) -> Other
                                                        end),
       meck:new(bucos, [passthrough]),
       meck:expect(bucos, run, fun(_, _) -> {ok, []} end),
       ok
   end,
   fun(_) ->
       meck:unload(filelib),
       meck:unload(file),
       meck:unload(image_optimizer),
       meck:unload(bucos),
       ok
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, "test1.png"}, image_optimizer:optimize("test1.png")
          ),
        ?assertEqual(
           {ok, "test1.jpg"}, image_optimizer:optimize("test1.jpg")
          ),
        ?assertEqual(
           {ok, "test1.jpeg"}, image_optimizer:optimize("test1.jpeg")
          ),
        ?assertEqual(
           {ok, "test1.gif"}, image_optimizer:optimize("test1.gif")
          ),
        ?assertEqual(
           {ok, "test1.svg"}, image_optimizer:optimize("test1.svg")
          ),
        ?assertEqual(
           {error, invalid_file}, image_optimizer:optimize("test1.not_an_image")
          ),
        ?assertEqual(
           {ok, <<"test1.png">>}, image_optimizer:optimize(<<"test1.png">>)
          ),
        ?assertEqual(
           {ok, <<"test1.jpg">>}, image_optimizer:optimize(<<"test1.jpg">>)
          ),
        ?assertEqual(
           {ok, <<"test1.jpeg">>}, image_optimizer:optimize(<<"test1.jpeg">>)
          ),
        ?assertEqual(
           {ok, <<"test1.gif">>}, image_optimizer:optimize(<<"test1.gif">>)
          ),
        ?assertEqual(
           {ok, <<"test1.svg">>}, image_optimizer:optimize(<<"test1.svg">>)
          ),
        ?assertEqual(
           {error, invalid_file}, image_optimizer:optimize(<<"test1.not_an_image">>)
          ),
        ?assertEqual(
           {ok, "test1-output.png"}, image_optimizer:optimize(<<"test1.png">>, [{output, "test1-output.png"}])
          ),
        ?assertEqual(
           {ok, "test1-output.jpg"}, image_optimizer:optimize(<<"test1.jpg">>, [{output, "test1-output.jpg"}])
          ),
        ?assertEqual(
           {ok, "test1-output.jpeg"}, image_optimizer:optimize(<<"test1.jpeg">>, [{output, "test1-output.jpeg"}])
          ),
        ?assertEqual(
           {ok, "test1-output.gif"}, image_optimizer:optimize(<<"test1.gif">>, [{output, "test1-output.gif"}])
          ),
        ?assertEqual(
           {ok, "test1-output.svg"}, image_optimizer:optimize(<<"test1.svg">>, [{output, "test1-output.svg"}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.png">>}, image_optimizer:optimize(<<"test1.png">>, [{output, <<"test1-output.png">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.jpg">>}, image_optimizer:optimize(<<"test1.jpg">>, [{output, <<"test1-output.jpg">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.jpeg">>}, image_optimizer:optimize(<<"test1.jpeg">>, [{output, <<"test1-output.jpeg">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.gif">>}, image_optimizer:optimize(<<"test1.gif">>, [{output, <<"test1-output.gif">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.svg">>}, image_optimizer:optimize(<<"test1.svg">>, [{output, <<"test1-output.svg">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.png">>}, image_optimizer:optimize("test1.png", [{output, <<"test1-output.png">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.jpg">>}, image_optimizer:optimize("test1.jpg", [{output, <<"test1-output.jpg">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.jpeg">>}, image_optimizer:optimize("test1.jpeg", [{output, <<"test1-output.jpeg">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.gif">>}, image_optimizer:optimize("test1.gif", [{output, <<"test1-output.gif">>}])
          ),
        ?assertEqual(
           {ok, <<"test1-output.svg">>}, image_optimizer:optimize("test1.svg", [{output, <<"test1-output.svg">>}])
          )
    end
   ]}.

missing_optimizer_test_() ->
  {setup,
   fun() ->
       meck:new(filelib, [passthrough, unstick]),
       meck:expect(filelib, is_regular, fun(_) -> true end),
       meck:new(image_optimizer, [passthrough]),
       meck:expect(image_optimizer, os_find_executable, fun (_) -> false end),
       ok
   end,
   fun(_) ->
       meck:unload(filelib),
       meck:unload(image_optimizer),
       ok
   end,
   [
    fun() ->
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize("test1.png")
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize("test1.jpg")
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize("test1.jpeg")
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize("test1.gif")
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize("test1.svg")
          ),
        ?assertEqual(
           {error, invalid_file}, image_optimizer:optimize("test1.not_an_image")
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize(<<"test1.png">>)
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize(<<"test1.jpg">>)
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize(<<"test1.jpeg">>)
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize(<<"test1.gif">>)
          ),
        ?assertEqual(
           {error, optimizer_not_found}, image_optimizer:optimize(<<"test1.svg">>)
          ),
        ?assertEqual(
           {error, invalid_file}, image_optimizer:optimize(<<"test1.not_an_image">>)
          )
    end
   ]}.
