defmodule Ellie.Repo do
  use Ecto.Repo, 
    otp_app: :ellie,
    adapter: Ecto.Adapters.Postgres

  def init(_, opts) do
    {:ok, opts}
  end
end
