defmodule Ellie.Tasks.Binstall do
  @moduledoc false

  def binstall(_args) do
    download_and_unpack(
      "https://github.com/alco/goon/releases/download/v1.1.1/goon_linux_386.tar.gz",
      "goon.tar.gz"
    )

    download_and_unpack(
      "https://github.com/elm-lang/elm-platform/releases/download/0.18.0-exp/elm-platform-linux-64bit.tar.gz",
      "0.18.0/platform.tar.gz"
    )

    download_and_unpack(
      "https://github.com/avh4/elm-format/releases/download/0.7.0-exp/elm-format-0.18-0.7.0-exp-linux-x64.tgz",
      "0.18.0/format.tar.gz"
    )

    download_and_unpack(
      "https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz",
      "0.19.1/elm.gz"
    )

    download_and_unpack(
      "https://github.com/avh4/elm-format/releases/download/0.8.2/elm-format-0.8.2-linux-x64.tgz",
      "0.19.1/format.tar.gz"
    )
  end

  defp download_and_unpack(url, dest) do
    priv_dir = Application.app_dir(:ellie, "priv")
    path = Path.join([priv_dir, "bin", dest])

    if not File.exists?(path) do
      IO.puts("==> Starting download of #{dest}")
      dir = Path.dirname(path)

      unpack_cmd =
        if String.ends_with?(dest, "elm.gz"),
          do: "gunzip #{path}",
          else: "tar xvzC #{dir} -f #{path}"

      File.mkdir_p!(dir)
      System.cmd("sh", ["-c", "curl -L #{url} --output #{path}"])
      System.cmd("sh", ["-c", unpack_cmd])
      System.cmd("sh", ["-c", "chmod +x #{dir}/*"])
    else
      IO.puts("==> Binary already exists: #{dest}")

      :ok
    end
  end
end
