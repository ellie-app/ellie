defmodule Ellie.Tasks.Binstall do
  @moduledoc false

  def binstall(_args) do
    download_and_unpack("https://github.com/alco/goon/releases/download/v1.1.1/goon_linux_386.tar.gz", "./priv/bin/goon.tar.gz")
    download_and_unpack("https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz", "./priv/bin/0.18.0/platform.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz", "./priv/bin/0.18.0/format.tar.gz")
    download_and_unpack("https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz", "./priv/bin/0.19.0/elm.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.8.0-rc3/elm-format-0.19-0.8.0-rc3-linux-x64.tgz", "./priv/bin/0.19.0/format.tar.gz")
  end

  defp download_and_unpack(url, dest) do
    if not File.exists?(dest) do
      dir = Path.dirname(dest)
      File.mkdir_p!(dir)
      System.cmd("sh", ["-c", "curl -L #{url} --output #{dest}"])
      System.cmd("sh", ["-c", "tar xvzC #{dir} -f #{dest}"])
      System.cmd("sh", ["-c", "chmod +x #{dir}/*"])
    else
      :ok
    end
  end
end

