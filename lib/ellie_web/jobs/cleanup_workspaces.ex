defmodule EllieWeb.Jobs.CleanupWorkspaces do
  def run() do
    Ellie.Domain.Workspace.cleanup()
  end
end
