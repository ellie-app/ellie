defmodule Ellie.Jobs.CleanupEmbeds do
  def run() do
    Ellie.Domain.Embed.cleanup(30)
  end
end
