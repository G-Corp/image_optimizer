# File: Image.Optimizer.ex
# This file was generated from image_optimizer.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Image.Optimizer do
  @callback optimize(any, any, any) :: any
  def unquote(:"optimize")(arg1) do
    :erlang.apply(:"image_optimizer", :"optimize", [arg1])
  end
  def unquote(:"optimize")(arg1, arg2) do
    :erlang.apply(:"image_optimizer", :"optimize", [arg1, arg2])
  end
end
