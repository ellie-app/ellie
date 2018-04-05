defmodule Ellie.Elm.Version do
  defstruct major: 1, minor: 0, patch: 0

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
