defmodule Ellie.Types.Settings do
  defstruct [:font_size, :font_family, :theme, :vim_mode]

  @behaviour Ecto.Type

  @type t :: %Ellie.Types.Settings{
    font_size: String.t,
    font_family: String.t,
    theme: String.t,
    vim_mode: boolean
  }

  def default() do
    %Ellie.Types.Settings{
      font_size: "14px",
      font_family: "monospace",
      theme: "DARK",
      vim_mode: false
    }
  end

  def up do
    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'theme') then
          create type theme as enum (
            'DARK',
            'LIGHT'
          );
        end if;
      end $$;
    """

    Ecto.Migration.execute """
      do $$ begin
        if not exists (select 1 from pg_type where typname = 'settings') then
          create type settings as (
            font_size   varchar(255),
            font_family varchar(255),
            theme       theme,
            vim_mode    boolean
          );
        end if;
      end $$;
    """
  end

  def down do
    Ecto.Migration.execute "drop type if exists settings;"
    Ecto.Migration.execute "drop type if exists theme;"
  end

  def type, do: :settings

  def cast(%__MODULE__{} = settings), do: {:ok, settings}
  def cast(_), do: :error

  def load(data) when is_tuple(data) do
    {font_size, font_family, theme, vim_mode} = data
    settings = %__MODULE__{font_size: font_size, font_family: font_family, theme: theme, vim_mode: vim_mode}
    {:ok, settings}
  end
  def load(_), do: :error

  def dump(%__MODULE__{} = settings) do
    data = {settings.font_size, settings.font_family, settings.theme, settings.vim_mode}
    {:ok, data}
  end
  def dump(_), do: :error
end
