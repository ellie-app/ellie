defmodule Ellie.Tasks do
  @moduledoc false

  defdelegate binstall(args), to: __MODULE__.Binstall
end

