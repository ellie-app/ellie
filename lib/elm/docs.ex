defmodule Elm.Docs do
  @type t :: list(Elm.Docs.Module.t)
end

defmodule Elm.Docs.Module do
  defstruct [:name, :comment, :unions, :aliases, :unions, :aliases, :values, :binops]
  @type t :: %Elm.Docs.Module{
    name: String.t,
    comment: String.t,
    unions: list(Elm.Docs.Union.t),
    aliases: list(Elm.Docs.Alias.t),
    values: list(Elm.Docs.Value.t),
    binops: list(Elm.Docs.Binop.t)
  }
end

defmodule Elm.Docs.Alias do
  defstruct [:name, :comment, :args, :type]
  @type t :: %Elm.Docs.Alias{
    name: String.t,
    comment: String.t,
    args: list(String.t),
    type: String.t
  }
end

defmodule Elm.Docs.Union do
  defstruct [:name, :comment, :args, :tags]
  @type t :: %Elm.Docs.Union{
    name: String.t,
    comment: String.t,
    args: list(String.t),
    tags: list({String.t, list(String.t)})
  }
end

defmodule Elm.Docs.Value do
  defstruct [:name, :comment, :type]
  @type t :: %Elm.Docs.Value{
    name: String.t,
    comment: String.t,
    type: String.t
  }
end

defmodule Elm.Docs.Binop do
  defstruct [:name, :comment, :type, :associativity, :precedence]
  @type associativity :: :left | :none | :right
  @type t :: %Elm.Docs.Binop{
    name: String.t,
    comment: String.t,
    type: String.t,
    associativity: associativity,
    precedence: integer
  }
end
