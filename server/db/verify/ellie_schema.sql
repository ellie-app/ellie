-- Verify ellie:ellie_schema on pg

begin;

select pg_catalog.has_schema_privilege('ellie', 'usage');

rollback;
