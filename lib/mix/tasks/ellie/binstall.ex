defmodule Mix.Tasks.Ellie.Binstall do
  use Mix.Task

  def run(_) do
    case :os.type do
      {:unix, :darwin} ->
        download_osx()
      _ ->
        download_linux()
    end
  end

  defp download_osx() do
    download_and_unpack("https://github.com/alco/goon/releases/download/v1.1.1/goon_darwin_386.tar.gz", "./priv/static/bin/goon.tar.gz")
    download_and_unpack("https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-macos.tar.gz", "./priv/bin/0.18.0/platform.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-mac-x64.tgz", "./priv/bin/0.18.0/format.tar.gz")
    download(System.get_env("ELM_19_BINARY_URL"), "./priv/bin/0.19.0/elm")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-mac-x64.tgz", "./priv/bin/0.19.0/format.tar.gz")
  end

  defp download_linux() do
    download_and_unpack("https://github.com/alco/goon/releases/download/v1.1.1/goon_linux_386.tar.gz", "./priv/static/bin/goon.tar.gz")
    download_and_unpack("https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz", "./priv/bin/0.18.0/platform.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz", "./priv/bin/0.18.0/format.tar.gz")
    download_and_unpack(System.get_env("ELM_19_BINARY_URL"), "./priv/bin/0.19.0/elm.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz", "./priv/bin/0.19.0/format.tar.gz")
  end

  defp download_and_unpack(url, dest) do
    dir = Path.dirname(dest)
    File.mkdir_p!(dir)
    Mix.Shell.cmd("curl -L #{url} --output #{dest}", [], fn a -> a end)
    Mix.Shell.cmd("tar xvzC #{dir} -f #{dest}", [], fn a -> a end)
    Mix.Shell.cmd("rm #{dest}", [], fn a -> a end)
  end

  defp download(url, dest) do
    File.mkdir_p!(Path.dirname(dest))
    Mix.Shell.cmd("curl -L #{url} --output #{dest}", [], fn a -> a end)
  end
end
