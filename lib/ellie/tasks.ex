defmodule Ellie.Tasks do
  @moduledoc false

  defdelegate binstall(args), to: __MODULE__.Binstall

  defdelegate migrate(args), to: __MODULE__.Migrate
end
