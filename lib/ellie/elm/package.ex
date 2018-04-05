defmodule Ellie.Elm.Package do
  defstruct name: %Ellie.Elm.Name{}, version: %Ellie.Elm.Version{}

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

  def load(data) when is_tuple(data) do
    {name, version} = data
    packge = %Ellie.Elm.Package{name: Ellie.Elm.Name.load(name), version: Ellie.Elm.Version.load(version)}
    {:ok, packge}
  end
  def load(_), do: :error

  def dump(%Ellie.Elm.Package{} = package) do
    data = {Ellie.Elm.Name.dump(package.name), Ellie.Elm.Version.dump(package.version)}
    {:ok, data}
  end
  def dump(_), do: :error
end
