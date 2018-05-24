defmodule Elm.Ecto.Package do
  alias Elm.Package

  @behaviour Ecto.Type

  def up do
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_package') then
          create type elm_package as (
            name    elm_name,
            version elm_version
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists elm_package;"
  end

  def type, do: :elm_package

  def cast(%Package{} = package), do: {:ok, package}
  def cast(_), do: :error

  def load({name_data, version_data}) do
    with {:ok, name} <- Elm.Ecto.Name.load(name_data),
         {:ok, version} <- Elm.Ecto.Version.load(version_data)
    do
      {:ok, %Package{name: name, version: version}}
    else
      _ -> :error
    end
  end
  def load(_), do: :error

  def dump(%Package{name: name, version: version}) do
    with {:ok, name_data} <- Elm.Ecto.Name.dump(name),
         {:ok, version_data} <- Elm.Ecto.Version.dump(version)
    do
      {:ok, {name_data, version_data}}
    else
      _ -> :error
    end
  end
  def dump(_), do: :error
end
