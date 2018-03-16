-- Revert ellie:revision_functions from pg

begin;

drop function ellie.retrieve_revision(json);
drop function ellie.create_revision(json);
drop function ellie.update_revision(json);

commit;
