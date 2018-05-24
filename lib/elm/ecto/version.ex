defmodule Elm.Ecto.Version do
  alias Elm.Version

  @behaviour Ecto.Type

  def up do
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_version') then
          create type elm_version as (
            major   int,
            minor   int,
            patch   int
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists elm_version;"
  end

  def type, do: :elm_version

  def cast(%Version{} = version), do: {:ok, version}
  def cast(_), do: :error

  def load({major, minor, patch}) when is_integer(major) and is_integer(minor) and is_integer(patch) do
    {:ok, Version.create(major, minor, patch)}
  end
  def load(_), do: :error

  def dump(%Version{major: major, minor: minor, patch: patch}) do
    {:ok, {major, minor, patch}}
  end
  def dump(_), do: :error
end
