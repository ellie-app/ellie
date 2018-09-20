defmodule Ellie.Tasks.Binstall do
  @moduledoc false

  def binstall(_args) do
    download_and_unpack("https://github.com/alco/goon/releases/download/v1.1.1/goon_linux_386.tar.gz", "goon.tar.gz")
    download_and_unpack("https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz", "0.18.0/platform.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz", "0.18.0/format.tar.gz")
    download_and_unpack("https://github.com/elm/compiler/releases/download/0.19.0/binaries-for-linux.tar.gz", "0.19.0/elm.tar.gz")
    download_and_unpack("https://github.com/avh4/elm-format/releases/download/0.8.0-rc3/elm-format-0.19-0.8.0-rc3-linux-x64.tgz", "0.19.0/format.tar.gz")
  end

  defp download_and_unpack(url, dest) do
    priv_dir = Application.app_dir(:ellie, "priv")
    path = Path.join([priv_dir, "bin", dest])

    if not File.exists?(path) do
      IO.puts "==> Starting download of #{dest}"
      dir = Path.dirname(path)

      File.mkdir_p!(dir)
      System.cmd("sh", ["-c", "curl -L #{url} --output #{path}"])
      System.cmd("sh", ["-c", "tar xvzC #{dir} -f #{path}"])
      System.cmd("sh", ["-c", "chmod +x #{dir}/*"])
    else
      IO.puts "==> Binary already exists: #{dest}"

      :ok
    end
  end
end

