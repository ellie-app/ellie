defmodule EllieWeb.Graphql.Resolvers.CreateRevision do
  alias Ellie.Elm.{Version, Package}
  alias Ellie.{Repo, Revision}

  def call(args, %{context: %{current_user: current_user}}) do
    data = args[:inputs]
    Repo.insert(%Revision{
      title: data.title,
      elm_code: data.elm_code,
      html_code: data.html_code,
      packages: Enum.map(data.packages, &Kernel.struct!(Package, &1)),
      terms_version: data.terms_version,
      elm_version: %Version{ major: 0, minor: 19, patch: 0 },
      user: current_user
    })
  end
  
  def call(_, _) do
    {:error, "Bad createRevision call"}
  end
end
