defmodule Elm.Ecto.Name do
  alias Elm.Name

  @behaviour Ecto.Type

  def up do
    Ecto.Migration.execute("""
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'elm_name') then
          create type elm_name as (
            username   varchar(255),
            project    varchar(255)
          );
        end if;
      end $$;
    """)
  end

  def down do
    Ecto.Migration.execute("drop type if exists elm_name;")
  end

  def type, do: :elm_name

  def cast(%Name{} = name), do: {:ok, name}
  def cast(_), do: :error

  def load({user, project}) when is_binary(user) and is_binary(project) do
    {:ok, %Name{user: user, project: project}}
  end

  def load(_), do: :error

  def dump(%Name{user: user, project: project}) do
    {:ok, {user, project}}
  end

  def dump(_), do: :error
end
