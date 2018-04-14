defmodule Ellie.Elm.Package do
  alias Ellie.Elm.Version

  defstruct name: %Ellie.Elm.Name{}, version: %Ellie.Elm.Version{}

  def docs_view_link(%Ellie.Elm.Package{} = package) do
    Application.get_env(:ellie, :package_endpoint) <>
      "/" <>
      package.name.user <>
      "/" <>
      package.name.project <>
      "/" <>
      Version.to_string(package.version)
  end

  def docs_data_link(%Ellie.Elm.Package{} = package) do
    docs_view_link(package) <> "/docs.json"
  end

  @behaviour Ecto.Type

  def up do
    Ellie.Elm.Version.up
    Ellie.Elm.Name.up
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_package') then
          create type elm_package as (
            name     elm_name,
            version  elm_version
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists elm_package;"
    Ellie.Elm.Name.down
    Ellie.Elm.Version.down
  end

  def type, do: :elm_package

  def cast(%Ellie.Elm.Package{} = package), do: {:ok, package}
  def cast(_), do: :error

  def load({name, version}) do
    with {:ok, n} <- Ellie.Elm.Name.load(name),
      {:ok, v} <- Ellie.Elm.Version.load(version)
    do
      {:ok, %Ellie.Elm.Package{name: n, version: v}}
    else
      _ -> :error
    end
  end
  def load(_), do: :error

  def dump(%Ellie.Elm.Package{} = package) do
    with {:ok, name} <- Ellie.Elm.Name.dump(package.name),
      {:ok, version} <- Ellie.Elm.Version.dump(package.version)
    do
      {:ok, {name, version}}
    else
      _ -> :error
    end
  end
  def dump({{_user, _project}, {_major,_minor,_patch}} = stuff) do
    {:ok, stuff}
  end
  def dump(_), do: :error
end
