defmodule EllieWeb.OembedView do
  use EllieWeb, :view
  alias Ellie.Types.ProjectId

  def render("index.json", %{revision: revision, width: width, height: height}) do
    %{
      version: "1.0",
      type: "rich",
      provider_name: "ellie-app.com",
      provider_url: "https://ellie-app.com",
      height: height,
      width: width,
      title: revision.title,
      html: "<iframe src=\"https://ellie-app.com/embed/#{ProjectId.to_string(revision.id)}\" width=#{width} height=#{height} frameBorder=\"0\" allowtransparency=\"true\"></iframe>",
    }
  end
end
