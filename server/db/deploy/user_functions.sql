-- Deploy ellie:user_functions to pg
-- requires: users

begin;

create function ellie.retrieve_user(json) returns json as $$
	declare
    inputs alias for $1;
		result record;
	begin
		select *
      into result
  	  from ellie.users
		  where id = (inputs->>'id')::uuid
		  limit 1;

    return
      case
        when result is null then null
        else row_to_json(result)
      end;
	end;
$$ language plpgsql stable;

create function ellie.create_user(json) returns json as $$
  declare
    inputs alias for $1;
    result record;
  begin
    insert into ellie.users
      default values
      returning * into result;

    return row_to_json(result);
  end;
$$ language plpgsql volatile;

create function ellie.update_user(json) returns json as $$
  declare
    inputs alias for $1;
    user_data record;
    result record;
  begin
    select *
      into user_data
      from json_to_record(inputs)
      as x( id uuid
          , terms_version int 
          , settings ellie.settings
          );

    update ellie.users
      set terms_version = user_data.terms_version,
          settings = user_data.settings,
          updated_at = default
      where id = user_data.id
      returning *
      into result;

    return row_to_json(result);
  end;
$$ language plpgsql volatile;

commit;
