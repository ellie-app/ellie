defmodule EllieWeb.Graphql.Plugins.Sentry do
  @behaviour Absinthe.Phase
  @behaviour Absinthe.Plugin

  def before_resolution(execution), do: execution
  def after_resolution(execution), do: execution

  def pipeline(pipeline, execution) do
    if Map.has_key?(execution.context, :original) do
      pipeline ++ [__MODULE__]
    else
      pipeline
    end
  end

  def run(blueprint, _opts) do
    case blueprint.execution.result.errors do
      [] ->
        {:ok, blueprint}

      errors ->
        Enum.each(errors, fn error ->
          Sentry.capture_message(error.message,
            extra: %{
              document: Map.get(blueprint.execution.context, :original),
              message: error.message,
              locations: Enum.map(error.locations, &Map.from_struct/1),
              path: error.path,
              extra: error.extra
            }
          )
        end)

        {:ok, blueprint}
    end
  end
end
