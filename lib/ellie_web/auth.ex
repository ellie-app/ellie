defmodule EllieWeb.Auth do
  alias Ellie.Repo
  alias Ellie.Types.User

  def verify(token) do
    case Phoenix.Token.verify(EllieWeb.Endpoint, "user auth", token, max_age: :infinity) do
      {:ok, user_id} ->
        User
          |> Repo.get_by(id: user_id)
          |> case do
            nil ->  {:error, :user_not_found}
            user -> {:ok, user}
          end
      error ->
        error
    end
  end

  def sign(user) do
    Phoenix.Token.sign(EllieWeb.Endpoint, "user auth", user.id)
  end
end
