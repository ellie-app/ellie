defmodule EllieWeb.Graphql.Resolvers.CreateRevision do
  alias Ellie.Elm.{Version, Package}
  alias Ellie.{Repo, Revision}

  def call(%{inputs: data}, %{context: %{current_user: current_user}}) do
    case current_user.terms_version do
      nil ->
        {:error, "Cannot save revision without accepting terms of service"}
      terms_version ->
        Repo.insert(%Revision{
          title: Map.get(data, :title),
          elm_code: data.elm_code,
          html_code: data.html_code,
          packages: Enum.map(data.packages, &Kernel.struct!(Package, &1)),
          terms_version: terms_version,
          elm_version: %Version{ major: 0, minor: 19, patch: 0 },
          user: current_user
        })
    end
  end

  def call(_, _) do
    {:error, "Bad createRevision call"}
  end
end
