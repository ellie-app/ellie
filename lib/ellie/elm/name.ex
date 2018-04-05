defmodule Ellie.Elm.Name do
  defstruct user: "user", project: "project"

  @behaviour Ecto.Type
  
  def up do
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_name') then
          create type elm_name as (
            username  varchar(255),
            project   varchar(255)
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists elm_name;"
  end

  def type, do: :elm_name
  
  def cast(%Ellie.Elm.Name{} = name), do: {:ok, name}
  def cast(_), do: :error

  def load(data) when is_tuple(data) do
    {user, project} = data
    name = %Ellie.Elm.Name{user: user, project: project}
    {:ok, name}
  end
  def load(_), do: :error

  def dump(%Ellie.Elm.Name{} = name) do
    data = {name.user, name.project}
    {:ok, data}
  end
  def dump(_), do: :error
end
