defmodule Ellie.Jobs.CleanupLogs do
  alias Ellie.Helpers.EnumHelpers

  def run() do
    with {:ok, issue_ids} <- get_old_issue_ids(),
         :ok <- delete_issues(issue_ids)
    do
      :ok
    else
      {:ok, []} ->
        :ok
      {:error, message} ->
        Sentry.capture_message "Ellie.Jobs.CleanupLogs: #{message}",
          stacktrace: System.stacktrace()
        :error
      _otherwise ->
        Sentry.capture_message "Ellie.Jobs.CleanupLogs: unknown failure"
    end
  end

  defp get_old_issue_ids() do
    url = "https://sentry.io/api/0/projects/ellie/ellie/issues/?query=age:%2B26d&statsPeriod="
    with {:ok, %HTTPoison.Response{status_code: 200, body: body}} <- HTTPoison.get(url, headers()),
         {:ok, data} <- Poison.decode(body)
    do
      EnumHelpers.traverse_result(data, &Map.fetch(&1, "id"))
    else
      _error ->
        {:error, "Could not fetch old issues from Sentry"}
    end
  end

  defp delete_issues(ids) do
    case ids do
      [] ->
        :ok
      some_ids ->
        ids_query = some_ids |> Enum.map(fn id -> "id=#{id}" end) |> Enum.join("&")
        url = "https://sentry.io/api/0/projects/ellie/ellie/issues/?" <> ids_query
        case HTTPoison.delete(url, headers()) do
          {:ok, %HTTPoison.Response{status_code: 204}} ->
            :ok
          _error ->
            {:error, "Could not remove old issues from Sentry"}
        end
    end
  end

  defp headers() do
    [{"Authorization", "Bearer #{System.get_env("SENTRY_API_KEY")}"}]
  end
end
