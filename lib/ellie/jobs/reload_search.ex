defmodule Ellie.Jobs.ReloadSearch do
  def run() do
    Ellie.Domain.Search.reload()
  end
end
