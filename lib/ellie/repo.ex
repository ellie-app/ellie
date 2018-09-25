defmodule Ellie.Repo do
  use Ecto.Repo, otp_app: :ellie
  def init(_, opts) do
    {:ok, opts}
  end
end
