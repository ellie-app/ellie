defmodule Ellie.Search do
  def search(query) do
    Ellie.Search.Server.search(Ellie.Search.Server.server, query)
  end
end
