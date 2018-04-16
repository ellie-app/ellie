defmodule Ellie.Search.Helpers do
  import Ecto.Query
  alias Ellie.Elm.Searchable
  alias Ellie.Repo
  alias Ellie.Helpers.EnumHelpers

  defmacrop compute_score(query) do
    quote do
      fragment(
        "((similarity((name).username, ?) * 3) + (similarity((name).project, ?) * 2) + similarity(summary, ?))",
        unquote(query),
        unquote(query),
        unquote(query)
      )
    end
  end

  def search(query, threshold) do
    sub = from s in Searchable,
      select: %{ s | score: compute_score(^query) }

    Repo.all(
      from e in subquery(sub),
        where: e.score > ^threshold,
        limit: 5,
        order_by: [desc: e.score]
    )
    |> Enum.map(&Searchable.to_package/1)
  end

  def reload() do
    case download() do
      {:ok, searchables} ->
        Repo.transaction fn ->
          Repo.delete_all(Searchable)
          Repo.insert_all(Searchable, searchables, on_conflict: :nothing)
          Repo.one(from s in Searchable, select: count(s.name))
        end
      error ->
        error
    end
  end

  defp download() do
    url = Application.get_env(:ellie, :package_endpoint) <> "/search.json"
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        case EnumHelpers.traverse_result(Poison.decode!(body), &Searchable.from_json/1) do
          {:ok, stuff} -> {:ok, Enum.map(stuff, &Searchable.to_map/1)}
          error -> error
        end
      {:ok, %HTTPoison.Response{status_code: code}} ->
        {:error, "Bad response from the package repository: #{code}"}
      {:error, error} ->
        {:error, HTTPoison.Error.message(error)}
    end
  end
end
