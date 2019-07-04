defmodule Elm.Package do
  alias Elm.Version
  alias Elm.Name

  defstruct [:name, :version]

  @type t :: %Elm.Package{name: Name.t(), version: Version.t()}
end
