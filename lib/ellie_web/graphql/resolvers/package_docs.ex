defmodule EllieWeb.Graphql.Resolvers.PackageDocs do
  alias Ellie.Elm.Package
  alias Ellie.Elm.Docs
  alias Ellie.Repo
  alias Absinthe.Resolution.Helpers

  def call(package, _, %{context: %{loader: loader}}) do
    loader
      |> Dataloader.load(Ellie.Elm.Docs, Docs, package)
      |> Helpers.on_load(fn loader ->
        case Dataloader.get(loader, Ellie.Elm.Docs, Docs, package) do
          nil -> fetch_and_save(package)
          docs -> {:ok, docs.docs_0_19_0}
        end
      end)
  end

  defp fetch_and_save(package) do
    case HTTPoison.get(Package.docs_data_link(package)) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        docs = Poison.decode!(body)
        changeset = %Docs{package: package, docs_0_19_0: docs}
        Repo.insert(changeset)
        {:ok, docs}
      {:ok, %HTTPoison.Response{status_code: code}} ->
        {:error, "Bad response from the package repository: #{code}"}
      {:error, error} ->
        {:error, HTTPoison.Error.message(error)}
    end
  end
end
