defmodule Ellie.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def up do
    Elm.Ecto.Version.up()
    Elm.Ecto.Name.up()
    Elm.Ecto.Package.up()
  end

  def down do
    Elm.Ecto.Package.down()
    Elm.Ecto.Name.down()
    Elm.Ecto.Version.down()
  end
end
