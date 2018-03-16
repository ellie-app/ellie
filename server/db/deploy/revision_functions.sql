-- Deploy ellie:revision_functions to pg
-- requires: revisions

begin;

create function ellie.retrieve_revision(json) returns json as $$
  declare
    inputs alias for $1;
    result record;
  begin
    select *
      into result
      from ellie.revisions
      where project_id = (inputs->>'project_id')::uuid
      and revision_number = (inputs->>'revision_number')::int
      limit 1;

    return
      case
        when result is null then null
        else row_to_json(result)
      end;
  end;
$$ language plpgsql stable;

create function ellie.create_revision(json) returns json as $$
  declare
    inputs alias for $1;
    revision_data record;
    result record;
  begin
    select *
      into revision_data
      from json_to_record(inputs)
      as x( title varchar(255)
          , elm_code text
          , html_code text
          , packages ellie.package[]
          , elm_version ellie.version
          , terms_version int
          );

    insert into ellie.revisions
      (revision_number, title, elm_code, html_code, packages, elm_version, terms_version)
      values ( 0
             , revision_data.title
             , revision_data.elm_code
             , revision_data.html_code
             , revision_data.packages
             , revision_data.elm_version
             , revision_data.terms_version
             )
      returning *
      into result;

    return row_to_json(result);
  end;
$$ language plpgsql volatile;

create function ellie.update_revision(json) returns json as $$
  declare
    inputs alias for $1;
    record_data record;
    result record;
  begin
    select *
      into record_data
      from json_to_record(inputs)
      as x( project_id uuid
          , revision_number int
          , title varchar(255)
          , elm_code text
          , html_code text
          , packages ellie.package[]
          , elm_version ellie.version
          , terms_version int
          );

    insert into ellie.revisions
      (project_id, revision_number, title, elm_code, html_code, packages, elm_version, terms_version)
      values ( record_data.project_id
             , record_data.revision_number + 1
             , record_data.title
             , record_data.elm_code
             , record_data.html_code
             , record_data.packages
             , record_data.elm_version
             , record_data.terms_version
             )
      returning *
      into result;

    return row_to_json(result);
  end;
$$ language plpgsql volatile;

commit;
