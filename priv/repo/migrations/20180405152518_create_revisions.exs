defmodule Ellie.Repo.Migrations.CreateRevisions do
  use Ecto.Migration
  alias Ellie.Types.PrettyId

  @our_epoch 1_524_347_380_348
  @shard_id 100

  def up do
    execute "create sequence id_generator_sequence;"

    execute """
      create or replace function id_generator(out result bigint) as $$
      declare
          our_epoch bigint := #{@our_epoch};
          seq_id bigint;
          now_millis bigint;
          shard_id int := #{@shard_id};
      begin
          SELECT nextval('id_generator_sequence') % 1024 INTO seq_id;
          SELECT FLOOR(EXTRACT(EPOCH FROM clock_timestamp()) * 1000) INTO now_millis;
          result := (now_millis - our_epoch) << 23;
          result := result | (shard_id << 10);
          result := result | (seq_id);
      end;
      $$ language plpgsql;
    """

    create table(:revisions, primary_key: false) do
      add :id,              PrettyId.type,                   primary_key: true, default: fragment("id_generator()")
      add :title,           :string
      add :elm_code,        :text,                           null: false
      add :html_code,       :text,                           null: false
      add :packages,        {:array, Elm.Ecto.Package.type}, null: false
      add :elm_version,     Elm.Ecto.Version.type,           null: false
      add :terms_version,   :integer
      timestamps(updated_at: false)
    end
  end

  def down do
    drop_if_exists table(:revisions)
    execute "drop sequence if exists id_generator_sequence;"
    execute "drop function if exists id_generator(bigint);"
  end
end
