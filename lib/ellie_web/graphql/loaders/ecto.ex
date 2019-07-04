defmodule EllieWeb.Graphql.Loaders.Ecto do
  defstruct [
    :repo,
    :query,
    :run_batch,
    repo_opts: [],
    batches: %{},
    results: %{},
    default_params: %{},
    options: []
  ]

  @type t :: %__MODULE__{
          repo: Ecto.Repo.t(),
          query: query_fun,
          repo_opts: Keyword.t(),
          batches: map,
          results: map,
          default_params: map,
          run_batch: batch_fun,
          options: Keyword.t()
        }

  @type query_fun :: (Ecto.Queryable.t(), any -> Ecto.Queryable.t())
  @type batch_fun :: (Ecto.Queryable.t(), Ecto.Query.t(), [any], Keyword.t() -> [any])
  @type opt ::
          {:query, query_fun}
          | {:repo_opts, Keyword.t()}
          | {:timeout, pos_integer}

  import Ecto.Query

  @doc """
  Create an Ecto Dataloader source.

  This module handles retrieving data from Ecto for dataloader. It requires a
  valid Ecto Repo. It also accepts a `repo_opts:` option which is handy for
  applying options to any calls to Repo functions that this module makes.

  For example, you can use this module in a multi-tenant context by using
  the `prefix` option:

  ```
  Dataloader.Ecto.new(MyApp.Repo, repo_opts: [prefix: "tenant"])
  ```
  """
  @spec new(Ecto.Repo.t(), [opt]) :: t
  def new(repo, opts \\ []) do
    data =
      opts
      |> Keyword.put_new(:query, &query/2)
      |> Keyword.put_new(:run_batch, &run_batch(repo, &1, &2, &3, &4, &5))

    opts = Keyword.take(opts, [:timeout])

    %__MODULE__{repo: repo, options: opts}
    |> struct(data)
  end

  @doc """
  Default implementation for loading a batch. Handles looking up records by
  column
  """
  def run_batch(repo, _queryable, query, col, inputs, repo_opts) do
    results = load_rows(col, inputs, query, repo, repo_opts)
    grouped_results = group_results(results, col)

    for value <- inputs do
      grouped_results
      |> Map.get(value, [])
      |> Enum.reverse()
    end
  end

  defp load_rows({:typed, col, typestruct}, inputs, query, repo, repo_opts) do
    query
    |> where([q], field(q, ^col) in fragment("?", type(^inputs, {:array, ^typestruct})))
    |> repo.all(repo_opts)
  end

  defp load_rows(col, inputs, query, repo, repo_opts) do
    query
    |> where([q], field(q, ^col) in ^inputs)
    |> repo.all(repo_opts)
  end

  defp group_results(results, col) do
    results
    |> Enum.reduce(%{}, fn result, grouped ->
      value = Map.get(result, col_to_key(col))
      Map.update(grouped, value, [result], &[result | &1])
    end)
  end

  defp col_to_key({:typed, col, _}), do: col
  defp col_to_key(col), do: col

  defp query(schema, _) do
    schema
  end

  defimpl Dataloader.Source do
    def run(source) do
      results =
        source.batches
        |> Dataloader.pmap(
          &run_batch(&1, source),
          timeout: source.options[:timeout] || 15_000,
          tag: "Ecto batch"
        )

      results =
        Map.merge(source.results, results, fn _, v1, v2 ->
          Map.merge(v1, v2)
        end)

      %{source | results: results, batches: %{}}
    end

    def fetch(%{results: results} = source, batch, item) do
      batch = normalize_key(batch, source.default_params)
      {batch_key, item_key, _item} = get_keys(batch, item)

      with {:ok, batch} <- Map.fetch(results, batch_key) do
        Map.fetch(batch, item_key)
      end
    end

    def put(source, _batch, _item, %Ecto.Association.NotLoaded{}) do
      source
    end

    def put(source, batch, item, result) do
      batch = normalize_key(batch, source.default_params)
      {batch_key, item_key, _item} = get_keys(batch, item)

      results =
        Map.update(
          source.results,
          batch_key,
          %{item_key => result},
          &Map.put(&1, item_key, result)
        )

      %{source | results: results}
    end

    def load(source, batch, item) do
      case fetch(source, batch, item) do
        :error ->
          batch = normalize_key(batch, source.default_params)
          {batch_key, item_key, item} = get_keys(batch, item)
          entry = {item_key, item}

          update_in(source.batches, fn batches ->
            Map.update(batches, batch_key, MapSet.new([entry]), &MapSet.put(&1, entry))
          end)

        _ ->
          source
      end
    end

    def pending_batches?(%{batches: batches}) do
      batches != %{}
    end

    defp chase_down_queryable([field], schema) do
      case schema.__schema__(:association, field) do
        %{queryable: queryable} ->
          queryable

        %Ecto.Association.HasThrough{through: through} ->
          chase_down_queryable(through, schema)

        val ->
          raise """
          Valid association #{field} not found on schema #{inspect(schema)}
          Got: #{inspect(val)}
          """
      end
    end

    defp chase_down_queryable([field | fields], schema) do
      case schema.__schema__(:association, field) do
        %{queryable: queryable} ->
          chase_down_queryable(fields, queryable)
      end
    end

    defp is_ecto_type(structure) do
      [Ecto.Type] in Keyword.values(Keyword.take(structure.__info__(:attributes), [:behaviour]))
    end

    defp get_keys_type({queryable, opts}, %structure{} = value) do
      case normalize_value(queryable, value) do
        {:primary, col, value} ->
          {{:queryable, self(), queryable, :one, {:typed, col, structure}, opts}, value, value}

        _ ->
          raise "cardinality required unless using primary key"
      end
    end

    defp get_keys({assoc_field, opts}, %schema{} = record) when is_atom(assoc_field) do
      if is_ecto_type(schema) do
        get_keys_type({assoc_field, opts}, record)
      else
        primary_keys = schema.__schema__(:primary_key)
        id = Enum.map(primary_keys, &Map.get(record, &1))
        queryable = chase_down_queryable([assoc_field], schema)
        {{:assoc, schema, self(), assoc_field, queryable, opts}, id, record}
      end
    end

    defp get_keys({{cardinality, queryable}, opts}, value) when is_atom(queryable) do
      {_, col, value} = normalize_value(queryable, value)
      {{:queryable, self(), queryable, cardinality, col, opts}, value, value}
    end

    defp get_keys({queryable, opts}, value) when is_atom(queryable) do
      case normalize_value(queryable, value) do
        {:primary, col, value} ->
          {{:queryable, self(), queryable, :one, col, opts}, value, value}

        _ ->
          raise "cardinality required unless using primary key"
      end
    end

    defp get_keys(key, item) do
      raise """
      Invalid: #{inspect(key)}
      #{inspect(item)}

      The batch key must either be a schema module, or an association name.
      """
    end

    defp normalize_value(queryable, [{col, value}]) do
      case queryable.__schema__(:primary_key) do
        [^col] ->
          {:primary, col, value}

        _ ->
          {:not_primary, col, value}
      end
    end

    defp normalize_value(queryable, value) do
      [primary_key] = queryable.__schema__(:primary_key)
      {:primary, primary_key, value}
    end

    # This code was totally OK until cardinalities showed up. Now it's ugly :(
    # It is however correct, which is nice.
    @cardinalities [:one, :many]

    defp normalize_key({cardinality, queryable}, default_params)
         when cardinality in @cardinalities do
      normalize_key({{cardinality, queryable}, []}, default_params)
    end

    defp normalize_key({cardinality, queryable, params}, default_params)
         when cardinality in @cardinalities do
      normalize_key({{cardinality, queryable}, params}, default_params)
    end

    defp normalize_key({key, params}, default_params) do
      {key, Enum.into(params, default_params)}
    end

    defp normalize_key(key, default_params) do
      {key, default_params}
    end

    defp run_batch(
           {{:queryable, pid, queryable, cardinality, col, opts} = key, entries},
           source
         ) do
      inputs = Enum.map(entries, &elem(&1, 0))

      query = source.query.(queryable, opts)

      repo_opts = Keyword.put(source.repo_opts, :caller, pid)

      cardinality_mapper = cardinality_mapper(cardinality, queryable)

      results =
        queryable
        |> source.run_batch.(query, col, inputs, repo_opts)
        |> Enum.map(cardinality_mapper)

      results =
        inputs
        |> Enum.zip(results)
        |> Map.new()

      {key, results}
    end

    defp run_batch({{:assoc, schema, pid, field, queryable, opts} = key, records}, source) do
      {ids, records} = Enum.unzip(records)

      query = source.query.(queryable, opts)
      query = Ecto.Queryable.to_query(query)

      repo_opts = Keyword.put(source.repo_opts, :caller, pid)

      empty = schema |> struct |> Map.fetch!(field)

      results =
        records
        |> Enum.map(&Map.put(&1, field, empty))
        |> source.repo.preload([{field, query}], repo_opts)
        |> Enum.map(&Map.get(&1, field))

      {key, Map.new(Enum.zip(ids, results))}
    end

    defp cardinality_mapper(:many, _) do
      fn value ->
        value
      end
    end

    defp cardinality_mapper(:one, queryable) do
      fn
        [] -> nil
        [value] -> value
        other -> raise Ecto.MultipleResultsError, queryable: queryable, count: length(other)
      end
    end
  end
end
