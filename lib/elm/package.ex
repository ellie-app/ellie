defmodule Elm.Package do
  alias Elm.Version
  alias Elm.Name

  defstruct [:name, :version]

  @type t :: %Elm.Package{name: Name.t, version: Version.t}

  def docs_view_link(%Elm.Package{} = package) do
    System.get_env("PACKAGE_SITE") <>
      "/packages/" <>
      package.name.user <>
      "/" <>
      package.name.project <>
      "/" <>
      Version.to_string(package.version)
  end

  def docs_data_link(%Elm.Package{} = package) do
    docs_view_link(package) <> "/docs.json"
  end
end
