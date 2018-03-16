-- Deploy ellie:revisions to pg
-- requires: ellie_schema
-- requires: uuid_extension

begin;

create type ellie.version as (
  major   int,
  minor   int,
  patch   int
);

create type ellie.name as (
  username  varchar(127),
  project   varchar(127)
);

create type ellie.package as (
  name      ellie.name,
  version   ellie.version
);

create table ellie.revisions (
  project_id        uuid                          not null    default uuid_generate_v4(),
  revision_number   int                           not null,
  title             varchar(255),
  elm_code          text                          not null,
  html_code         text                          not null,
  packages          ellie.package[]               not null,
  elm_version       ellie.version                 not null,
  terms_version     int,
  created_at        timestamp without time zone   not null    default (now() at time zone 'utc'),
  primary key (project_id, revision_number)
);

commit;
