defmodule Ellie.Elm.Version do
  defstruct major: 1, minor: 0, patch: 0

  def to_string(%Ellie.Elm.Version{major: major, minor: minor, patch: patch}) do
    Integer.to_string(major) <> "." <> Integer.to_string(minor) <> "." <> Integer.to_string(patch)
  end

  def from_string(value) do
    with [major_s, minor_s, patch_s] <- String.split(value, "."),
      {major, _} <- Integer.parse(major_s),
      {minor, _} <- Integer.parse(minor_s),
      {patch, _} <- Integer.parse(patch_s)
    do
      {:ok, %Ellie.Elm.Version{major: major, minor: minor, patch: patch}}
    else
      _ -> :error
    end
  end

  @behaviour Ecto.Type

  def up do
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_version') then
          create type elm_version as (
            major  int,
            minor  int,
            patch  int
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists elm_version;"
  end

  def type, do: :elm_version

  def cast(%Ellie.Elm.Version{} = version), do: {:ok, version}
  def cast(_), do: :error

  def load(data) when is_tuple(data) do
    {major, minor, patch} = data
    version = %Ellie.Elm.Version{major: major, minor: minor, patch: patch}
    {:ok, version}
  end
  def load(_), do: :error

  def dump(%Ellie.Elm.Version{} = version) do
    data = {version.major, version.minor, version.patch}
    {:ok, data}
  end
  def dump(_), do: :error
end

defimpl Poison.Decoder, for: Ellie.Elm.Version do
  def decode(value, _options) do
    case Ellie.Elm.Version.from_string(value) do
      :error -> {:error, {:invalid, "Expecting a version MAJOR.MINOR.PATCH"}}
      success -> success
    end
  end
end
