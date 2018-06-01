defmodule EllieWeb.OembedView do
  use EllieWeb, :view

  def render("index.json", %{revision: revision, width: width, height: height}) do
    %{
      version: "1.0",
      type: "rich",
      provider_name: "ellie-app.com",
      provider_url: "https://ellie-app.com",
      height: height,
      width: width,
      title: revision.title,
      html: "<iframe src=\"https://ellie-app.com/embed/#{to_string(revision.id)}\" width=#{width} height=#{height} frameBorder=\"0\" allowtransparency=\"true\"></iframe>",
    }
  end
end
