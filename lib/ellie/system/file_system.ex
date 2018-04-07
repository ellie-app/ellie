defmodule Ellie.System.FileSystem do
  def create_temporary_directory() do
    case System.tmp_dir() do
      nil -> :error
      path -> {:ok, path}
    end
  end

  def write(path, data) do
    File.write(path, data)
  end

  def read(path) do
    File.read(path)
  end

  def remove_directory(path) do
    File.rm_rf(path)
  end
end
