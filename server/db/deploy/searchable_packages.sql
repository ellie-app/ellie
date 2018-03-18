-- Deploy ellie:searchable_packages to pg
-- requires: revisions

begin;

create extension if not exists pg_trgm;

create table ellie.searchable_packages (
  name      ellie.name        not null,
  summary   varchar(255)      not null,
  versions  ellie.version[]   not null,
  primary key (name)
);

create index searchable_packages_trgm_index_on_summary
  on ellie.searchable_packages
  using gin (summary gin_trgm_ops);

create index searchable_packages_trgm_index_on_name_username
  on ellie.searchable_packages
  using gin (((name).username) gin_trgm_ops);

create index searchable_packages_trgm_index_on_name_project
  on ellie.searchable_packages
  using gin (((name).project) gin_trgm_ops);

create function ellie.searchable_packages_populate(json) returns json as $$
  declare
    inputs alias for $1;
  begin
    truncate ellie.searchable_packages;
    insert into ellie.searchable_packages
      select *
        from json_populate_recordset(null::ellie.searchable_packages, inputs->'packages');
    return null;
  end;
$$ language plpgsql volatile;

create function ellie.searchable_packages_search(json) returns json as $$
  declare
    inputs alias for $1;
    result json;
  begin
    select coalesce(json_agg(t), '[]')
      into result
      from
        ( with scored_packages as (
            select
                *,
                ( (similarity((name).username, inputs->>'query') * 3) +
                  (similarity((name).project, inputs->>'query') * 2) +
                  similarity(summary, inputs->>'query')
                ) as score
              from ellie.searchable_packages
          )
          select name, summary, versions
            from scored_packages
            where score >= (inputs->>'threshold')::real
            order by score desc
            limit 5
        ) t;
    return result;
  end;
$$ language plpgsql stable;

create function ellie.searchable_packages_search_by_name(json) returns json as $$
  declare
    inputs alias for $1;
    result json;
  begin
    select coalesce(json_agg(t), '[]')
      into result
      from
        ( with scored_packages as (
            select
                *,
                ( similarity((name).username, inputs->>'user') +
                  similarity((name).project, inputs->>'project')
                ) as score
              from ellie.searchable_packages
          )
          select name, summary, versions
            from scored_packages
            order by score
            limit 5
        ) t;
    return result;
  end;
$$ language plpgsql stable;

commit;
