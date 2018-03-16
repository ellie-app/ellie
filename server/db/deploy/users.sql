-- Deploy ellie:users to pg
-- requires: ellie_schema
-- requires: uuid_extension

begin;

create type ellie.theme as enum ('DARK', 'LIGHT');

create type ellie.settings as (
  font_size      varchar(63),
  font_family    varchar(63),
  theme         ellie.theme,
  vim_mode       boolean
);

create table ellie.users (
  id              uuid                          not null    default uuid_generate_v4(),
  terms_version   int,
  settings        ellie.settings                not null    default row('14px', 'monospace', 'DARK', false)::ellie.settings,
  created_at      timestamp without time zone   not null    default (now() at time zone 'utc'),
  updated_at      timestamp without time zone   not null    default (now() at time zone 'utc'),
  primary key (id)
);

commit;
