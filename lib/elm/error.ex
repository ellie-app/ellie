defmodule Elm.Error do
  @type t ::
    {:general_problem, Elm.Error.GeneralProblem.t}
    | {:module_problems, list(Elm.Error.BadModule.t)}
end

defmodule Elm.Error.GeneralProblem do
  defstruct [:path, :title, :message]
  @type t :: %Elm.Error.GeneralProblem{
    path: Path.t,
    title: String.t,
    message: list(Elm.Error.Chunk.t)
  }
end

defmodule Elm.Error.BadModule do
  defstruct [:path, :name, :problems]
  @type t :: %Elm.Error.BadModule{
    path: Path.t,
    name: String.t,
    problems: list(Elm.Error.Problem.t)
  }
end

defmodule Elm.Error.Problem do
  defstruct [:title, :region, :message]
  @type t :: %Elm.Error.Problem{
    title: String.t,
    region: Elm.Error.Region.t,
    message: list(Elm.Error.Chunk.t)
  }
end

defmodule Elm.Error.Region do
  defstruct [:start, :end]
  @type t :: %Elm.Error.Region{
    start: Elm.Error.Position.t,
    end: Elm.Error.Position.t
  }
end

defmodule Elm.Error.Position do
  defstruct [:line, :column]
  @type t :: %Elm.Error.Position{
    line: integer,
    column: integer
  }
end

defmodule Elm.Error.Chunk do
  @type t ::
    {:styled, Elm.Error.Style.t, String.t}
    | {:unstyled, String.t}
end

defmodule Elm.Error.Style do
  defstruct [:bold, :underline, :color]

  @type t :: %Elm.Error.Style{
    bold: boolean,
    underline: boolean,
    color: Elm.Error.Color.t | nil
  }
end

defmodule Elm.Error.Color do
  @type t ::
    :red
    | :vivid_red
    | :magenta
    | :vivid_magenta
    | :yellow
    | :vivid_yellow
    | :green
    | :vivid_green
    | :cyan
    | :vivid_cyan
    | :blue
    | :vivid_blue
    | :white
    | :vivid_white
    | :black
    | :vivid_black

  @spec from_string(String.t) :: t | nil
  def from_string("red"), do: :red
  def from_string("RED"), do: :vivid_red
  def from_string("magenta"), do: :magenta
  def from_string("MAGENTA"), do: :vivid_magenta
  def from_string("yellow"), do: :yellow
  def from_string("YELLOW"), do: :vivid_yellow
  def from_string("green"), do: :green
  def from_string("GREEN"), do: :vivid_green
  def from_string("cyan"), do: :cyan
  def from_string("CYAN"), do: :vivid_cyan
  def from_string("blue"), do: :blue
  def from_string("BLUE"), do: :vivid_blue
  def from_string("white"), do: :white
  def from_string("WHITE"), do: :vivid_white
  def from_string("black"), do: :black
  def from_string("BLACK"), do: :vivid_black
  def from_string(_), do: nil

  @spec to_string(t) :: String.t
  def to_string(:red), do: "red"
  def to_string(:vivid_red), do: "RED"
  def to_string(:magenta), do: "magenta"
  def to_string(:vivid_magenta), do: "MAGENTA"
  def to_string(:yellow), do: "yellow"
  def to_string(:vivid_yellow), do: "YELLOW"
  def to_string(:green), do: "green"
  def to_string(:vivid_green), do: "GREEN"
  def to_string(:cyan), do: "cyan"
  def to_string(:vivid_cyan), do: "CYAN"
  def to_string(:blue), do: "blue"
  def to_string(:vivid_blue), do: "BLUE"
  def to_string(:white), do: "white"
  def to_string(:vivid_white), do: "WHITE"
  def to_string(:black), do: "black"
  def to_string(:vivid_black), do: "BLACK"
  def to_string(_), do: ""
end
