defmodule Ellie.Adapters.Search.Ecto do
  import Ecto.Query
  alias Ellie.Types.Searchable
  alias Ellie.Repo
  alias Elm.Platform

  @threshold 0.5

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

  def search(query) do
    sub = from s in Searchable,
      select: %{ s | score: compute_score(^query) }

    Repo.all(
      from e in subquery(sub),
        where: e.score > ^@threshold,
        limit: 5,
        order_by: [desc: e.score]
    )
    |> Enum.map(&Searchable.to_package/1)
  end

  def reload() do
    case Platform.search() do
      {:ok, searchables} ->
        Repo.transaction fn ->
          Repo.delete_all(Searchable)
          searchables
          |> Enum.map(&Map.from_struct/1)
          |> (&Repo.insert_all(Searchable, &1, on_conflict: :nothing)).()
          Repo.one(from s in Searchable, select: count(s.name))
        end
      :error ->
        :error
    end
  end
end
