defmodule Mix.Tasks.Ellie.Binstall do
  use Mix.Task

  def run(_) do
    download_and_unpack(
      "https://github.com/alco/goon/releases/download/v1.1.1/goon_darwin_386.tar.gz",
      "goon.tar.gz"
    )

    download_and_unpack(
      "https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-macos.tar.gz",
      "0.18.0/platform.tar.gz"
    )

    download_and_unpack(
      "https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-mac-x64.tgz",
      "0.18.0/format.tar.gz"
    )

    download_and_unpack(
      "https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-mac.tar.gz",
      "0.19.0/elm.tar.gz"
    )

    download_and_unpack(
      "https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-mac-x64.tgz",
      "0.19.0/format.tar.gz"
    )
  end

  defp download_and_unpack(url, dest) do
    priv_dir = Application.app_dir(:ellie, "priv")
    path = Path.join([priv_dir, "bin", dest])

    if not File.exists?(path) do
      dir = Path.dirname(path)

      File.mkdir_p!(dir)
      Mix.Shell.cmd("curl -L #{url} --output #{path}", [], fn a -> a end)
      Mix.Shell.cmd("tar xvzC #{dir} -f #{path}", [], fn a -> a end)
      Mix.Shell.cmd("chmod +x #{dir}/*", [], fn a -> a end)
    else
      :ok
    end
  end
end
