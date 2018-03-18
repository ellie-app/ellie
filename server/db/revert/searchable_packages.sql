-- Revert ellie:searchable_packages from pg

begin;

drop function ellie.searchable_packages_search_by_name(json);
drop function ellie.searchable_packages_search(json);
drop function ellie.searchable_packages_populate(json);
drop index ellie.searchable_packages_trgm_index_on_summary;
drop index ellie.searchable_packages_trgm_index_on_name_username;
drop index ellie.searchable_packages_trgm_index_on_name_project;
drop table ellie.searchable_packages;
drop extension if exists pg_trgm;

commit;
