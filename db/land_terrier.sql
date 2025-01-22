--
-- PostgreSQL database dump
--

-- Dumped from database version 16.3
-- Dumped by pg_dump version 16.3

-- Started on 2025-01-22 12:23:24

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 6245 (class 1262 OID 37534)
-- Name: land_terrier; Type: DATABASE; Schema: -; Owner: -
--

CREATE DATABASE land_terrier WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'English_United Kingdom.1252';


\connect land_terrier

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 6246 (class 0 OID 0)
-- Dependencies: 6245
-- Name: DATABASE land_terrier; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON DATABASE land_terrier IS 'Database for tracking land ownership and occupation that the Trust holds.';


--
-- TOC entry 6247 (class 0 OID 0)
-- Name: land_terrier; Type: DATABASE PROPERTIES; Schema: -; Owner: -
--

ALTER DATABASE land_terrier SET search_path TO '$user', 'public', 'postgis', 'extensions', 'domains', 'utils', 'qgis', 'metadata';


\connect land_terrier

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- TOC entry 9 (class 2615 OID 37535)
-- Name: domains; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA domains;


--
-- TOC entry 6248 (class 0 OID 0)
-- Dependencies: 9
-- Name: SCHEMA domains; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA domains IS 'Lookup domain tables.';


--
-- TOC entry 10 (class 2615 OID 37536)
-- Name: extensions; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA extensions;


--
-- TOC entry 6249 (class 0 OID 0)
-- Dependencies: 10
-- Name: SCHEMA extensions; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA extensions IS 'Schema for holding all extension functions and tables other than those for postgis which reside in their own schema. The schema must be in the search_path.';


--
-- TOC entry 11 (class 2615 OID 37537)
-- Name: land_admin; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA land_admin;


--
-- TOC entry 6250 (class 0 OID 0)
-- Dependencies: 11
-- Name: SCHEMA land_admin; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA land_admin IS 'Schema for land administration tables.';


--
-- TOC entry 12 (class 2615 OID 37538)
-- Name: metadata; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA metadata;


--
-- TOC entry 6251 (class 0 OID 0)
-- Dependencies: 12
-- Name: SCHEMA metadata; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA metadata IS 'Metadata and catalogue. The schema must be in the search_path.';


--
-- TOC entry 13 (class 2615 OID 37539)
-- Name: postgis; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA postgis;


--
-- TOC entry 6252 (class 0 OID 0)
-- Dependencies: 13
-- Name: SCHEMA postgis; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA postgis IS 'Spatial functions for PostgreSQL. The schema must be in the search_path.';


--
-- TOC entry 6253 (class 0 OID 0)
-- Dependencies: 7
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA public IS 'Standard public schema. The schema must be in the search_path.';


--
-- TOC entry 14 (class 2615 OID 37540)
-- Name: qgis; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA qgis;


--
-- TOC entry 6254 (class 0 OID 0)
-- Dependencies: 14
-- Name: SCHEMA qgis; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA qgis IS 'Projects and layer styles used by QGIS. The schema must be in the search_path.';


--
-- TOC entry 15 (class 2615 OID 37541)
-- Name: utils; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA utils;


--
-- TOC entry 6255 (class 0 OID 0)
-- Dependencies: 15
-- Name: SCHEMA utils; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA utils IS 'Schema for global functions, utilities, and shared resources. Similar to the public schema, but used for common utilities. The schema must be in the search_path.';


--
-- TOC entry 2 (class 3079 OID 37542)
-- Name: btree_gist; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS btree_gist WITH SCHEMA extensions;


--
-- TOC entry 6256 (class 0 OID 0)
-- Dependencies: 2
-- Name: EXTENSION btree_gist; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION btree_gist IS 'support for indexing common datatypes in GiST';


--
-- TOC entry 3 (class 3079 OID 38192)
-- Name: postgis; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS postgis WITH SCHEMA postgis;


--
-- TOC entry 6257 (class 0 OID 0)
-- Dependencies: 3
-- Name: EXTENSION postgis; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION postgis IS 'PostGIS geometry and geography spatial types and functions';


--
-- TOC entry 4 (class 3079 OID 39268)
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA extensions;


--
-- TOC entry 6258 (class 0 OID 0)
-- Dependencies: 4
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- TOC entry 1146 (class 1255 OID 39279)
-- Name: trf_log_parcel_history(); Type: FUNCTION; Schema: land_admin; Owner: -
--

CREATE FUNCTION land_admin.trf_log_parcel_history() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF NEW.valid_to IS NOT NULL THEN
        INSERT INTO land_admin.parcel_history (
            parcel_uuid, site_id, valid_from, valid_to, geom
        ) VALUES (
			OLD.parcel_uuid, OLD.site_id, OLD.valid_from, NEW.valid_to, OLD.geom
        );
		
        NEW.valid_from := NEW.valid_to;
		NEW.valid_to := NULL;
    END IF;
	
    RETURN NEW;
END;
$$;


--
-- TOC entry 6259 (class 0 OID 0)
-- Dependencies: 1146
-- Name: FUNCTION trf_log_parcel_history(); Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON FUNCTION land_admin.trf_log_parcel_history() IS 'The inputted `valid_to` field becomes the new `valid_from`. A copy of the old values from the modified row are inserted into the `parcel_history` table. The new `valid_to` field is set to `NULL`.';


--
-- TOC entry 1120 (class 1255 OID 39280)
-- Name: get_financial_quarter(date); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_financial_quarter(input_date date) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
    month INT := EXTRACT(MONTH FROM input_date);
    quarter TEXT;
BEGIN
    IF month >= 4 AND month <= 6 THEN
        quarter := '1';
    ELSIF month >= 7 AND month <= 9 THEN
        quarter := '2';
    ELSIF month >= 10 AND month <= 12 THEN
        quarter := '3';
    ELSE
        quarter := '4';
    END IF;

	quarter := 'Q' || quarter;
	
    RETURN quarter;
END;
$$;


--
-- TOC entry 6260 (class 0 OID 0)
-- Dependencies: 1120
-- Name: FUNCTION get_financial_quarter(input_date date); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.get_financial_quarter(input_date date) IS 'For a given date, returns the financial quarter in the format `Q1`. The financial year runs from 1st April to 31st March.';


--
-- TOC entry 1073 (class 1255 OID 39281)
-- Name: get_financial_year(date); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_financial_year(input_date date) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
    year_start INT;
    year_end INT;
BEGIN
    IF EXTRACT(MONTH FROM input_date) >= 4 THEN
        year_start := EXTRACT(YEAR FROM input_date);
        year_end := year_start + 1;
    ELSE
        year_start := EXTRACT(YEAR FROM input_date) - 1;
        year_end := EXTRACT(YEAR FROM input_date);
    END IF;

    RETURN year_start || '/' || RIGHT(year_end::TEXT, 2);
END;
$$;


--
-- TOC entry 6261 (class 0 OID 0)
-- Dependencies: 1073
-- Name: FUNCTION get_financial_year(input_date date); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.get_financial_year(input_date date) IS 'For a given date, returns the financial year in the format `YYYY/YY`. The financial year runs from 1st April to 31st March.';


--
-- TOC entry 358 (class 1255 OID 39282)
-- Name: get_table_columns_info(text, text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_table_columns_info(schema_name text, table_name text) RETURNS TABLE(pg_schema text, pg_table text, pg_ordinal_position integer, pg_column_name text, pg_column_type text, pg_not_null boolean, pg_key_constraints text, pg_foreign_schema text, pg_foreign_table text, pg_constraint_def text, pg_column_alias text, pg_column_comment text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
BEGIN
    RETURN QUERY EXECUTE format(
	$Q$
	SELECT
	pg_schema,
	pg_table,
	pg_ordinal_position,
	pg_column_name,
	pg_column_type,
	pg_not_null,
	pg_key_constraints,
	pg_foreign_schema,
	pg_foreign_table,
	pg_constraint_def,
	TRIM(pg_column_parts[1]) AS pg_column_alias,
	TRIM(pg_column_parts[2]) AS pg_column_comment
FROM
	(
		SELECT
			ROW_NUMBER() OVER (
				PARTITION BY
					nsp.nspname::text,
					cl.relname::text,
					a.attnum
				ORDER BY
					c.contype nulls last
			) r,
			nsp.nspname::text AS pg_schema,
			cl.relname::text AS pg_table,
			a.attnum::int AS pg_ordinal_position,
			CASE
				WHEN c.contype = ANY (ARRAY['p'::"char", 'f'::"char"]) THEN pg_get_constraintdef(c.oid)::text
				ELSE NULL::text END
			pg_constraint_def,
			a.attname::text AS pg_column_name,
			REPLACE(FORMAT_TYPE(a.atttypid, a.atttypmod), ',', ', ') AS pg_column_type,
			a.attnotnull pg_not_null,
			CASE
				WHEN c.contype = ANY (ARRAY['p'::"char", 'f'::"char"]) THEN c.contype::text
				ELSE NULL::text
			END AS pg_key_constraints,
			clref.relnamespace::regnamespace::text AS pg_foreign_schema,
			clref.relname::text AS pg_foreign_table,
			REGEXP_MATCH(
				COL_DESCRIPTION(a.attrelid, a.attnum::integer),
				'^(?:<<([^>]+)>>\s*)?(.+)$'
			) AS pg_column_parts
		FROM
			pg_attribute a
			LEFT JOIN pg_constraint c ON a.attrelid = c.conrelid
			AND (a.attnum = ANY (c.conkey))
			LEFT JOIN pg_class cl ON a.attrelid = cl.oid
			LEFT JOIN pg_class clref ON c.confrelid = clref.oid
			LEFT JOIN pg_namespace nsp ON cl.relnamespace = nsp.oid
		WHERE
			a.attnum > 0
			AND NOT a.attisdropped
			AND nsp.nspname::text = %L
			AND cl.relname::text = %L
		ORDER BY
			nsp.nspname,
			cl.relname,
			a.attnum
	)
WHERE
	r = 1
	$Q$, schema_name, table_name);
END;
$_$;


--
-- TOC entry 6262 (class 0 OID 0)
-- Dependencies: 358
-- Name: FUNCTION get_table_columns_info(schema_name text, table_name text); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.get_table_columns_info(schema_name text, table_name text) IS 'Returns the column infomation with comments for a given table.';


--
-- TOC entry 685 (class 1255 OID 39283)
-- Name: get_table_constraints_info(text, text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_table_constraints_info(schema_name text, table_name text) RETURNS TABLE(pg_schema text, pg_table text, pg_constraint_name text, pg_constraint_type text, pg_constraint_alias text, pg_constraint_comment text, pg_constraint_def text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
BEGIN
    RETURN QUERY EXECUTE format(
	$Q$
SELECT pg_schema,
pg_table,
pg_constraint_name,
pg_constraint_type,
TRIM(pg_comment_parts[1]) pg_constraint_alias,
CASE
	WHEN pg_constraint_type = 'f' THEN COALESCE(TRIM(pg_comment_parts[2]), 'Maintains referential integrity.')
	WHEN pg_constraint_type = 'p' THEN COALESCE(TRIM(pg_comment_parts[2]), 'Record identifier. Must be UNIQUE and NOT NULL.')
	ELSE TRIM(pg_comment_parts[2]) END pg_constraint_comment,
pg_constraint_def
FROM
(SELECT
	nsp.nspname::text pg_schema,
	rel.relname::text pg_table,
	conname::text pg_constraint_name,
	contype::text pg_constraint_type,
	REGEXP_MATCH(obj_description(con.oid), '^(?:<<([^>]+)>>\s*)?(.+)$') pg_comment_parts,
	PG_GET_CONSTRAINTDEF(con.oid)::text pg_constraint_def
FROM
	pg_catalog.pg_constraint con
	INNER JOIN pg_catalog.pg_class rel ON rel.oid = con.conrelid
	INNER JOIN pg_catalog.pg_namespace nsp ON nsp.oid = connamespace
WHERE
	nsp.nspname = %L
	AND rel.relname = %L
	) AS q
	ORDER BY pg_schema, pg_table,
	
		CASE pg_constraint_type
      WHEN 'p' THEN 1
      WHEN 'f' THEN 2
      WHEN 'u' THEN 3
      WHEN 'c' THEN 4
	  WHEN 'x' THEN 5
	  WHEN 't' THEN 6
      ELSE 7
   END, pg_constraint_name;
		$Q$, schema_name, table_name);
END;
$_$;


--
-- TOC entry 6263 (class 0 OID 0)
-- Dependencies: 685
-- Name: FUNCTION get_table_constraints_info(schema_name text, table_name text); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.get_table_constraints_info(schema_name text, table_name text) IS 'Returns the constraints present on a given table as well as comments.';


--
-- TOC entry 1078 (class 1255 OID 39284)
-- Name: get_table_info(text, text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_table_info(schema_name text, table_name text) RETURNS TABLE(pg_schema text, pg_table text, pg_table_type text, pg_table_alias text, pg_table_comment text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
BEGIN
    RETURN QUERY EXECUTE format(
	$Q$
SELECT pg_schema, pg_table, pg_table_type, TRIM(pg_table_parts[1]) pg_table_alias, TRIM(pg_table_parts[2]) pg_table_comment

FROM

(

SELECT nsp.nspname::text AS pg_schema, cl.relname::text pg_table, relkind::text pg_table_type,
REGEXP_MATCH(obj_description(cl.oid), '^(?:<<([^>]+)>>\s*)?(.+)$') pg_table_parts
FROM pg_class cl
LEFT JOIN pg_namespace nsp ON cl.relnamespace = nsp.oid
WHERE nsp.nspname::text = %L
AND relname::text = %L

) AS q;
$Q$, schema_name, table_name);
END;
$_$;


--
-- TOC entry 6264 (class 0 OID 0)
-- Dependencies: 1078
-- Name: FUNCTION get_table_info(schema_name text, table_name text); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.get_table_info(schema_name text, table_name text) IS 'Returns the table comments.';


--
-- TOC entry 1083 (class 1255 OID 39285)
-- Name: get_table_privileges_info(text, text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_table_privileges_info(schema_name text, table_name text) RETURNS TABLE(pg_schema text, pg_table text, pg_privilege_type text, pg_privilege_description text, pg_privilege_grantees text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
BEGIN
    RETURN QUERY EXECUTE format(
	$Q$
-- Recursive Common Table Expression (CTE) to build the role inheritance hierarchy
WITH RECURSIVE role_hierarchy AS (
    -- Base case: Start with all direct role memberships
    SELECT
        roleid AS parent_role,
        roleid AS child_role
    FROM
        pg_auth_members
    UNION ALL
    -- Recursive step: Add inherited roles to the hierarchy
    SELECT
        rh.parent_role,
        pam.roleid AS child_role
    FROM
        role_hierarchy rh
    JOIN
        pg_auth_members pam ON rh.child_role = pam.member
),

-- Remove duplicate role relationships and exclude system roles (pg_*)
distinct_role_hierarchy AS (
    SELECT DISTINCT 
        parent_role::regrole::text, 
        child_role::regrole::text 
    FROM 
        role_hierarchy
    WHERE 
        parent_role::regrole::text !~ 'pg_*'
),

-- Fetch privileges directly granted to roles for the specified table and schema
role_privileges AS (
    SELECT
        grantee AS role_name,
        privilege_type,
        table_schema,
        table_name
    FROM
        information_schema.role_table_grants
    WHERE
        table_schema = %1$L 
        AND table_name = %2$L
        AND grantee <> 'postgres' -- Exclude the default superuser role
),

-- Combine direct and inherited privileges by joining with the role hierarchy
all_privileges AS (
    SELECT
        rp.privilege_type,
        parent_role AS role_name -- Map privileges to the parent roles
    FROM
        role_privileges rp
    JOIN 
        distinct_role_hierarchy 
    ON 
        child_role = rp.role_name
),

-- Group privileges by type and aggregate role names as comma-separated lists
grouped_privileges AS (
    SELECT
        privilege_type,
        STRING_AGG(role_name, ', ' ORDER BY role_name) AS grantees
    FROM
        all_privileges
    GROUP BY
        privilege_type
)

-- Final query: Map privileges to descriptions and include grantees
SELECT
    table_schema AS pg_schema,
    table_name AS pg_table,
    privilege_type AS pg_privilege_type,
    privilege_description AS pg_privilege_description,
    COALESCE(grantees, '[Administrator Only]') AS pg_privilege_grantees
FROM
    -- Define privilege types and their descriptions
    (VALUES
          (1, 'SELECT', 'Allowed to view the data in the table.')
        , (2, 'INSERT', 'Allowed to add new rows of data.')
        , (3, 'UPDATE', 'Allowed to modify the data in existing rows.')
        , (4, 'DELETE', 'Allowed to delete rows of data from the table.')
    ) AS privs (priv_order, privilege_type, privilege_description)
    CROSS JOIN (
        VALUES
            (%1$L, %2$L) -- Specify the table and schema
    ) AS sch_tbl (table_schema, table_name)
LEFT JOIN 
    grouped_privileges 
USING (privilege_type)
ORDER BY 
    priv_order; -- Ensure consistent ordering of privilege types
	$Q$, schema_name, table_name);
END;
$_$;


--
-- TOC entry 364 (class 1255 OID 39286)
-- Name: get_table_triggers_info(text, text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.get_table_triggers_info(schema_name text, table_name text) RETURNS TABLE(pg_schema text, pg_table text, pg_trigger_name text, pg_trigger_comment text, pg_trigger_timing text, pg_trigger_events text, pg_trigger_columns text, pg_trigger_function text, pg_trigger_function_comment text)
    LANGUAGE plpgsql SECURITY DEFINER
    AS $_$
BEGIN
    RETURN QUERY EXECUTE format(
	$Q$
SELECT pg_schema,
pg_table,
pg_trigger_name,
pg_trigger_comment,
pg_trigger_timing,
pg_trigger_events,
CASE WHEN pg_trigger_columns = '' THEN NULL ELSE pg_trigger_columns END AS pg_trigger_columns,
pg_trigger_function,
pg_trigger_function_comment
FROM
(SELECT 
    nsp.nspname::text AS pg_schema,
    tbl.relname::text AS pg_table,
    trg.tgname::text AS pg_trigger_name,
    obj_description(trg.oid, 'pg_trigger') AS pg_trigger_comment,
		    CASE
        WHEN (trg.tgtype & 1) = 1 THEN 'BEFORE'
        ELSE 'AFTER'
    END AS pg_trigger_timing,
    array_to_string(ARRAY[
        CASE WHEN (trg.tgtype & 4) = 4 THEN 'INSERT' END,
        CASE WHEN (trg.tgtype & 8) = 8 THEN 'DELETE' END,
        CASE WHEN (trg.tgtype & 16) = 16 THEN 'UPDATE' END,
        CASE WHEN (trg.tgtype & 32) = 32 THEN 'TRUNCATE' END
    ], ' OR ') AS pg_trigger_events,
	        array_to_string(
        ARRAY(
            SELECT att.attname
            FROM unnest(trg.tgattr) AS colnum
            JOIN pg_attribute att ON att.attnum = colnum AND att.attrelid = trg.tgrelid
        ), ', '
    ) AS pg_trigger_columns,
	fn_nsp.nspname || '.' ||  proc.proname AS pg_trigger_function,
    obj_description(proc.oid, 'pg_proc') AS pg_trigger_function_comment
FROM pg_trigger trg
JOIN pg_class tbl ON trg.tgrelid = tbl.oid
JOIN pg_namespace nsp ON tbl.relnamespace = nsp.oid
JOIN pg_proc proc ON trg.tgfoid = proc.oid
JOIN pg_namespace fn_nsp ON proc.pronamespace = fn_nsp.oid
WHERE nsp.nspname = %L
	 AND tbl.relname = %L
	 AND
	trg.tgname !~ '^RI_'
	ORDER BY pg_schema, pg_table, pg_trigger_name) AS q;
	$Q$, schema_name, table_name);
END;
$_$;


--
-- TOC entry 893 (class 1255 OID 39287)
-- Name: st_areaha(postgis.geometry, boolean); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.st_areaha(geom postgis.geometry, rnd boolean DEFAULT false) RETURNS numeric
    LANGUAGE plpgsql
    AS $$
DECLARE

area_ha_numeric numeric;
area_ha numeric;

BEGIN

area_ha_numeric := st_area(geom)::numeric/1e4;

IF rnd
	THEN area_ha := round(area_ha_numeric, 4);
	ELSE area_ha := area_ha_numeric;
END IF;

RETURN area_ha;
END;
$$;


--
-- TOC entry 6265 (class 0 OID 0)
-- Dependencies: 893
-- Name: FUNCTION st_areaha(geom postgis.geometry, rnd boolean); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.st_areaha(geom postgis.geometry, rnd boolean) IS 'Calculates area in ha. Rounds to 4 dp (nearest m2) when rnd is TRUE.';


--
-- TOC entry 1081 (class 1255 OID 39288)
-- Name: st_geomfromgridref(text); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.st_geomfromgridref(gridref text) RETURNS postgis.geometry
    LANGUAGE plpgsql
    AS $_$
DECLARE
    parts text[];
	e_part int; n_part int;
    l1 integer; l2 integer;
    e100km integer; n100km integer;
    easting text; northing text;
BEGIN

	parts := regexp_matches(replace(gridref, ' ', ''),  '^([S,N,H,J,O,T])([A,B,C,D,E,F,G,H,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z])(\d*?)$');

	 -- abandon.. not a grid ref
     IF parts IS NULL OR array_length(parts, 1) = 0 OR length(parts[3]) % 2 <> 0 THEN
        RAISE NOTICE 'Invalid grid reference: %', gridref;
        RETURN NULL; 
     END IF;

	 	
    IF length(parts[3]) = 0 then
		e_part := 0;
		n_part := 0;
	ELSE
		e_part := left(parts[3], length(parts[3])/2);
		n_part := right(parts[3], length(parts[3])/2);
    end if;

	 


--    // get numeric values of letter references, mapping A->0, B->1, C->2, etc:
--    var l1 = gridref.toUpperCase().charCodeAt(0) - 'A'.charCodeAt(0);
--    var l2 = gridref.toUpperCase().charCodeAt(1) - 'A'.charCodeAt(0);
--    // shuffle down letters after 'I' since 'I' is not used in grid:
--    if (l1 > 7) l1--;
--    if (l2 > 7) l2--;
	  l1 := ascii(parts[1]) - ascii('A');
      l2 := ascii(parts[2]) - ascii('A');
      IF l1 > 7 THEN l1 := l1 - 1; END IF;
      IF l2 > 7 THEN l2 := l2 - 1; END IF;
   
--    // convert grid letters into 100km-square indexes from false origin (grid square SV):
--    var e100km = ((l1-2)%5)*5 + (l2%5);
--    var n100km = (19-Math.floor(l1/5)*5) - Math.floor(l2/5);

	  e100km := ((l1-2)%5)*5 + (l2%5);
      n100km := (19-floor(l1/5)*5) - floor(l2/5);
      
      IF (e100km<0 or e100km>6 or n100km<0 or n100km>12) THEN
      	RAISE EXCEPTION 'Invalid grid reference: %', grid_ref;
      END IF;
      
      easting  := e100km::text || rpad(e_part::text, 5, '0');
      northing := n100km::text || rpad(n_part::text, 5, '0');
      
      
      
RETURN ST_GeomFROMEWKT('SRID=27700;POINT(' || easting || ' ' || northing ||  ')');
END
$_$;


--
-- TOC entry 6266 (class 0 OID 0)
-- Dependencies: 1081
-- Name: FUNCTION st_geomfromgridref(gridref text); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.st_geomfromgridref(gridref text) IS 'Converts a geometry from a grid reference. It will raise a notice for any invalid grid references. It does not matter whether the input grid reference has spaces within it or whether they are of mixed precision.';


--
-- TOC entry 499 (class 1255 OID 39289)
-- Name: st_gridref(postgis.geometry, integer, boolean, boolean); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.st_gridref(geom postgis.geometry, max_figs integer DEFAULT 6, var_figs boolean DEFAULT false, include_spaces boolean DEFAULT false) RETURNS character varying
    LANGUAGE plpgsql
    AS $_$
        DECLARE
            e integer;
            n integer;
            e100k integer;
            n100k integer;
            l1 integer;
            l2 integer;
            letterPair text;
            -- needed to output left padded zeros
			e_char_zeros_n int;
			n_char_zeros_n int;
            e_char text;
            n_char text;
			figs int;
			nzeros int;
            gridref text;

        BEGIN

		IF max_figs % 2 <> 0 OR max_figs < 0 OR max_figs > 10 THEN
		RAISE EXCEPTION 'ERROR: max_figs must be and even number between 0 and 10';
		END IF;

                e := ST_X(st_centroid(geom))::integer;
                n := ST_Y(st_centroid(geom))::integer;

        -- get the 100km-grid indices
                e100k := floor(e/1e5);
                n100k := floor(n/1e5);

        --    translate those into numeric equivalents of the grid letters
                l1 := (19-n100k) - (19-n100k)%5 + floor((e100k+10)/5);
                l2 := (19-n100k)*5%25 + e100k%5;

        --   compensate for skipped 'I' and build grid letter-pairs
                IF l1 > 7 THEN l1 := l1 + 1; END IF;
                IF l2 > 7 THEN l2 := l2 + 1; END IF;

                letterPair := chr( ascii('A') + l1  ) || chr( ascii('A') + l2  );

        --    strip 100km-grid indices from easting & northing, and reduce precision
                e := floor(e%1e5);
                n := floor(n%1e5);


       -- pad eastings & northings with leading zeros (just in case, allow up to 16-digit (mm) refs)
       -- the padding is pretty important!
       -- e.g 346728, 800834 becomes NJ46728834 which should be NJ4672800834
       -- and 346997, 804999 becomes NJ469974999 which should be NJ4699704999
	   
	   -- return a variable figure precision (up to the max precision of max_figs)
	   -- e.g. TQ000000 --> TQ, SP450670 --> SP4567

				figs := (max_figs/2);
				
	   
                e_char := left(lpad(e::text, 5 ,'0'), figs);
                n_char := left(lpad(n::text, 5 ,'0'), figs);

				IF var_figs THEN

				e_char_zeros_n := length((regexp_matches(e_char,'(0+?)$')::text[])[1]);
				n_char_zeros_n := length((regexp_matches(n_char,'(0+?)$')::text[])[1]);

					IF e_char_zeros_n < n_char_zeros_n
					THEN nzeros := coalesce(e_char_zeros_n, 0);
					ELSE nzeros := coalesce(n_char_zeros_n, 0);
					END IF;			
				
				ELSE
				nzeros := 0;
				END IF;

				e_char := left(e_char, length(e_char) - nzeros);
				n_char := left(n_char, length(n_char) - nzeros);

				

IF include_spaces THEN gridref := trim(letterPair || ' ' || e_char || ' ' || n_char);
ELSE
gridref := letterPair || e_char ||  n_char;
END IF;
return gridref;
END
$_$;


--
-- TOC entry 6267 (class 0 OID 0)
-- Dependencies: 499
-- Name: FUNCTION st_gridref(geom postgis.geometry, max_figs integer, var_figs boolean, include_spaces boolean); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.st_gridref(geom postgis.geometry, max_figs integer, var_figs boolean, include_spaces boolean) IS 'Converts a geometry to OS Grid Reference. If the geometry is not a point then then geometric centroid is calculated first. https://gist.github.com/atph/0829d8d645720e679d8d04cbd7cfd5de';


--
-- TOC entry 1134 (class 1255 OID 39290)
-- Name: st_nearest_postcode(postgis.geometry); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.st_nearest_postcode(input_geom postgis.geometry) RETURNS text
    LANGUAGE plpgsql SECURITY DEFINER
    AS $$
DECLARE
    nearest_postcode TEXT;
BEGIN
    SELECT postcode INTO nearest_postcode
    FROM external_data.postcode_points pp
    ORDER BY input_geom <-> pp.geom ASC
    LIMIT 1;

    RETURN nearest_postcode;
END;
$$;


--
-- TOC entry 476 (class 1255 OID 39291)
-- Name: trf_update_areaha(); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.trf_update_areaha() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Update the "area_ha" field
    NEW.area_ha := st_areaha(NEW.geom, rnd := true);
  
    RETURN NEW;
END;
$$;


--
-- TOC entry 6268 (class 0 OID 0)
-- Dependencies: 476
-- Name: FUNCTION trf_update_areaha(); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.trf_update_areaha() IS 'Calculates `area_ha` to 4 d.p. from the geometry.';


--
-- TOC entry 372 (class 1255 OID 39292)
-- Name: trf_update_modified(); Type: FUNCTION; Schema: utils; Owner: -
--

CREATE FUNCTION utils.trf_update_modified() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    -- Update the "updated_at" to the current timestamp
    NEW.updated_at := NOW();
    
    -- Update the "updated_by" field to the current session user
    NEW.updated_by := current_user;
    
    RETURN NEW;
END;
$$;


--
-- TOC entry 6269 (class 0 OID 0)
-- Dependencies: 372
-- Name: FUNCTION trf_update_modified(); Type: COMMENT; Schema: utils; Owner: -
--

COMMENT ON FUNCTION utils.trf_update_modified() IS 'Sets `updated_at` and `updated_by` to the current datetime and user, respectively.';


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- TOC entry 230 (class 1259 OID 39293)
-- Name: dom_boundary_types; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_boundary_types (
    boundary_type_code text NOT NULL,
    boundary_type text NOT NULL,
    description text NOT NULL,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6270 (class 0 OID 0)
-- Dependencies: 230
-- Name: TABLE dom_boundary_types; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_boundary_types IS 'States how the red line boundary should be interpreted.';


--
-- TOC entry 231 (class 1259 OID 39298)
-- Name: dom_main_habitats; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_main_habitats (
    main_habitat_code text NOT NULL,
    main_habitat text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6271 (class 0 OID 0)
-- Dependencies: 231
-- Name: TABLE dom_main_habitats; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_main_habitats IS 'Main habitat for the site.';


--
-- TOC entry 232 (class 1259 OID 39303)
-- Name: dom_occupancy_types; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_occupancy_types (
    occupancy_type_code text NOT NULL,
    occupancy_type text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6272 (class 0 OID 0)
-- Dependencies: 232
-- Name: TABLE dom_occupancy_types; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_occupancy_types IS 'Specifies the type of land occupation.';


--
-- TOC entry 233 (class 1259 OID 39308)
-- Name: dom_ownership_types; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_ownership_types (
    ownership_type_code text NOT NULL,
    ownership_type text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6273 (class 0 OID 0)
-- Dependencies: 233
-- Name: TABLE dom_ownership_types; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_ownership_types IS 'Specifies types of land ownership.';


--
-- TOC entry 234 (class 1259 OID 39313)
-- Name: dom_renewal_types; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_renewal_types (
    renewal_type_code text NOT NULL,
    renewal_type text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6274 (class 0 OID 0)
-- Dependencies: 234
-- Name: TABLE dom_renewal_types; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_renewal_types IS 'Details what happens to the tenancy after the end date is passed.';


--
-- TOC entry 235 (class 1259 OID 39318)
-- Name: dom_site_categories; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_site_categories (
    site_category_code text NOT NULL,
    site_category text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6275 (class 0 OID 0)
-- Dependencies: 235
-- Name: TABLE dom_site_categories; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_site_categories IS 'Categorises the function of the site.';


--
-- TOC entry 236 (class 1259 OID 39323)
-- Name: dom_site_privacy; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_site_privacy (
    site_privacy_code text NOT NULL,
    site_privacy text NOT NULL,
    description text NOT NULL,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6276 (class 0 OID 0)
-- Dependencies: 236
-- Name: TABLE dom_site_privacy; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_site_privacy IS 'States, broadly, how the information can be shared on the common platform.';


--
-- TOC entry 237 (class 1259 OID 39333)
-- Name: dom_transfer_types; Type: TABLE; Schema: domains; Owner: -
--

CREATE TABLE domains.dom_transfer_types (
    transfer_type_code text NOT NULL,
    transfer_type text,
    description text,
    dom_order_id integer NOT NULL
);


--
-- TOC entry 6277 (class 0 OID 0)
-- Dependencies: 237
-- Name: TABLE dom_transfer_types; Type: COMMENT; Schema: domains; Owner: -
--

COMMENT ON TABLE domains.dom_transfer_types IS 'Details what happens to the tenancy after the end date is passed.';


--
-- TOC entry 238 (class 1259 OID 39338)
-- Name: occupancy; Type: TABLE; Schema: land_admin; Owner: -
--

CREATE TABLE land_admin.occupancy (
    occupancy_uuid uuid DEFAULT extensions.uuid_generate_v4() NOT NULL,
    ownership_uuid uuid NOT NULL,
    leasehold_title text,
    occupant text NOT NULL,
    occupancy_type_code text NOT NULL,
    occupancy_date_start date,
    occupancy_date_end date,
    occupancy_notes text,
    occupancy_file_notes json,
    occupancy_amount_gbp numeric,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_by text DEFAULT CURRENT_USER NOT NULL,
    renewal_type_code text NOT NULL,
    termination_date date,
    CONSTRAINT check_occupancy_end_after_start CHECK ((occupancy_date_end >= occupancy_date_start)),
    CONSTRAINT check_trust_has_start_date CHECK ((NOT ((occupancy_date_start IS NULL) AND (occupant = 'Trust'::text))))
);


--
-- TOC entry 6278 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_uuid IS 'Unique tracking id.';


--
-- TOC entry 6279 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.ownership_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.ownership_uuid IS 'Freehold tenure the lease is from.';


--
-- TOC entry 6280 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.leasehold_title; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.leasehold_title IS 'The leasehold title if applicable. This will be the official Land Reg title number.';


--
-- TOC entry 6281 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupant; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupant IS 'The entity in receipt of the lease. Must be ''Trust'' if the recipient is The Wildlife Trust.';


--
-- TOC entry 6282 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_type_code IS 'Details the type of lease populated from a set of valid values.';


--
-- TOC entry 6283 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_date_start; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_date_start IS 'Date (inclusive) of when the tenure started.';


--
-- TOC entry 6284 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_date_end; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_date_end IS 'Date (exclusive) of when the tenure ended/expired.';


--
-- TOC entry 6285 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_notes IS 'Notes relevant to the freehold.';


--
-- TOC entry 6286 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_file_notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_file_notes IS 'File notes and links to documentation.';


--
-- TOC entry 6287 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.occupancy_amount_gbp; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.occupancy_amount_gbp IS 'Total amount paid for the over the period.';


--
-- TOC entry 6288 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.updated_at; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.updated_at IS 'Indicates the last time the feature was updated.';


--
-- TOC entry 6289 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.updated_by; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.updated_by IS 'Indicates the user that last updated the feature.';


--
-- TOC entry 6290 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.renewal_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.renewal_type_code IS 'Categorises how to treat the occupancy after the expiration of the term.';


--
-- TOC entry 6291 (class 0 OID 0)
-- Dependencies: 238
-- Name: COLUMN occupancy.termination_date; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.occupancy.termination_date IS 'This is when the occupant ceases to have an affiliation under the current terms. This is necessary as it is possible for tenancies to ''hold over'' after the tenancy term ends. In this situation, the parcel will show as having a lease expired but still affiliated with the client. If a new lease is agreed, the termination date should end the daye before the new tenancy starts.';


--
-- TOC entry 6292 (class 0 OID 0)
-- Dependencies: 238
-- Name: CONSTRAINT check_occupancy_end_after_start ON occupancy; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT check_occupancy_end_after_start ON land_admin.occupancy IS 'Checks that the `occupancy_date_end` is after the `occupancy_date_start`';


--
-- TOC entry 6293 (class 0 OID 0)
-- Dependencies: 238
-- Name: CONSTRAINT check_trust_has_start_date ON occupancy; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT check_trust_has_start_date ON land_admin.occupancy IS 'Ensures that land parcels managed by The Trust have a commencement date.';


--
-- TOC entry 239 (class 1259 OID 39348)
-- Name: ownership; Type: TABLE; Schema: land_admin; Owner: -
--

CREATE TABLE land_admin.ownership (
    ownership_uuid uuid DEFAULT extensions.uuid_generate_v4() NOT NULL,
    parcel_uuid uuid NOT NULL,
    freehold_title text,
    landowner text NOT NULL,
    ownership_type_code text NOT NULL,
    ownership_date_start date,
    purchase_amount_gbp numeric,
    ownership_notes text,
    ownership_file_notes json,
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_by text DEFAULT CURRENT_USER NOT NULL,
    transfer_type_code text,
    CONSTRAINT check_trust_has_start_date CHECK ((NOT ((ownership_date_start IS NULL) AND (landowner = 'Trust'::text))))
);


--
-- TOC entry 6294 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.ownership_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.ownership_uuid IS 'Unique tracking id.';


--
-- TOC entry 6295 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.parcel_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.parcel_uuid IS 'References the parcel the tenure applies to.';


--
-- TOC entry 6296 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.freehold_title; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.freehold_title IS 'The freehold title if applicable. This will be the official Land Reg title number.';


--
-- TOC entry 6297 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.landowner; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.landowner IS 'Details who holds the freehold. The Wildlife Trust must be ''Trust''.';


--
-- TOC entry 6298 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.ownership_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.ownership_type_code IS 'Details the type of freehold tenure populated from a set of valid values.';


--
-- TOC entry 6299 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.ownership_date_start; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.ownership_date_start IS 'Date when the freehold started. For freeholds prior to The Trust from times in memoriam, this can be left blank. The freehold ends (through sale/disposal) with the addtitions of a subsequest rows `freehold_start_date`.';


--
-- TOC entry 6300 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.purchase_amount_gbp; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.purchase_amount_gbp IS 'Purchase ammout in GBP paid by the current freeholder (if known). Gifts should be `0`.';


--
-- TOC entry 6301 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.ownership_notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.ownership_notes IS 'Notes relevant to the ownership.';


--
-- TOC entry 6302 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.ownership_file_notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.ownership_file_notes IS 'File notes and links to documentation.';


--
-- TOC entry 6303 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.updated_at; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.updated_at IS 'Indicates the last time the feature was updated.';


--
-- TOC entry 6304 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.updated_by; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.updated_by IS 'Indicates the user that last updated the feature.';


--
-- TOC entry 6305 (class 0 OID 0)
-- Dependencies: 239
-- Name: COLUMN ownership.transfer_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.ownership.transfer_type_code IS 'Indicates how the parcel came into the possession of the landowner.';


--
-- TOC entry 6306 (class 0 OID 0)
-- Dependencies: 239
-- Name: CONSTRAINT check_trust_has_start_date ON ownership; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT check_trust_has_start_date ON land_admin.ownership IS 'Ensures that land parcels considered under The Trust''s holdings have a commencement date.';


--
-- TOC entry 240 (class 1259 OID 39357)
-- Name: parcel_history; Type: TABLE; Schema: land_admin; Owner: -
--

CREATE TABLE land_admin.parcel_history (
    parcel_history_uuid uuid DEFAULT extensions.uuid_generate_v4() NOT NULL,
    parcel_uuid uuid NOT NULL,
    site_id text NOT NULL,
    valid_from date,
    valid_to date NOT NULL,
    geom postgis.geometry(MultiPolygon,27700),
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_by text DEFAULT CURRENT_USER NOT NULL,
    notes text,
    CONSTRAINT check_parcel_has_valid_dates CHECK ((valid_to >= valid_from))
);


--
-- TOC entry 6307 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.parcel_history_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.parcel_history_uuid IS 'Unique identifier for the history record.';


--
-- TOC entry 6308 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.parcel_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.parcel_uuid IS 'Identifier of the parcel in the parcel table.';


--
-- TOC entry 6309 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.site_id; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.site_id IS 'References the site the parcel was a part of between the valid dates. Note this facilitates the transfer of parcels between sites and maintaining a history.';


--
-- TOC entry 6310 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.valid_from; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.valid_from IS 'Indicates when the information about the parcel should be considered valid from. Not that the deafult is for this to start as NULL indicating that up until the valid to date, the parcel was considered valid.';


--
-- TOC entry 6311 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.valid_to; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.valid_to IS 'Indicates the upper limit of when this version of the parcel data should be considered valid from. It should align equal teh valid_from date of the corresponding parcel on the parcels table.';


--
-- TOC entry 6312 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.geom; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.geom IS 'Represents the outer boundary of each parcel as a polygon feature.';


--
-- TOC entry 6313 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.updated_at; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.updated_at IS 'Indicates the last time the feature was updated.';


--
-- TOC entry 6314 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.updated_by; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.updated_by IS 'Indicates the user that last updated the feature.';


--
-- TOC entry 6315 (class 0 OID 0)
-- Dependencies: 240
-- Name: COLUMN parcel_history.notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcel_history.notes IS 'Free text field for notes about this version of the parcel history.';


--
-- TOC entry 6316 (class 0 OID 0)
-- Dependencies: 240
-- Name: CONSTRAINT check_parcel_has_valid_dates ON parcel_history; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT check_parcel_has_valid_dates ON land_admin.parcel_history IS 'Checks that the `valid_to` date is after the `valid_from` date.';


--
-- TOC entry 241 (class 1259 OID 39366)
-- Name: parcels; Type: TABLE; Schema: land_admin; Owner: -
--

CREATE TABLE land_admin.parcels (
    parcel_uuid uuid DEFAULT extensions.uuid_generate_v4() NOT NULL,
    site_id text NOT NULL,
    parcel_alias text NOT NULL,
    boundary_type_code text NOT NULL,
    notes text,
    area_ha numeric,
    valid_from date,
    valid_to date,
    geom postgis.geometry(MultiPolygon,27700),
    updated_at timestamp without time zone DEFAULT now() NOT NULL,
    updated_by text DEFAULT CURRENT_USER NOT NULL,
    CONSTRAINT check_parcel_has_valid_dates CHECK ((valid_to > valid_from))
);


--
-- TOC entry 6317 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.parcel_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.parcel_uuid IS 'Unique identifier for the parcel. It remains with the parcel forever and is used to maintain a historical record.';


--
-- TOC entry 6318 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.site_id; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.site_id IS 'Site that the parcel belong to. The geometry of the parcel will be a constituent part of the site geometry.';


--
-- TOC entry 6319 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.parcel_alias; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.parcel_alias IS '<<Parcel Alias>> An alias for the parcel to help distinguish it on a site. It must be unique within each site.';


--
-- TOC entry 6320 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.boundary_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.boundary_type_code IS '<<Boundary Type>> Indicates if the boundary is a proposed boundary,  Indicative boundary,  or legal boundary with valid values.';


--
-- TOC entry 6321 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.notes IS 'Free text for adding any notes specific to the parcel.';


--
-- TOC entry 6322 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.area_ha; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.area_ha IS 'Autogenerated, total area of the parcel in hectares.';


--
-- TOC entry 6323 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.valid_from; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.valid_from IS 'Indicates when the parcel in its current form can be considered valid from.';


--
-- TOC entry 6324 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.valid_to; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.valid_to IS 'Indicates when the parcel is valid to. NULL represents that the current parcel is valid. Adding in a valid_to date will archive a copy of the current parcel to the `parcel_history` table.';


--
-- TOC entry 6325 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.geom; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.geom IS 'Represents the outer boundary of each parcel as a polygon feature.';


--
-- TOC entry 6326 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.updated_at; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.updated_at IS 'Indicates the last time the feature was updated.';


--
-- TOC entry 6327 (class 0 OID 0)
-- Dependencies: 241
-- Name: COLUMN parcels.updated_by; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.parcels.updated_by IS 'Indicates the user that last updated the feature.';


--
-- TOC entry 6328 (class 0 OID 0)
-- Dependencies: 241
-- Name: CONSTRAINT check_parcel_has_valid_dates ON parcels; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT check_parcel_has_valid_dates ON land_admin.parcels IS 'Checks that the `valid_to` date is after the `valid_from` date.';


--
-- TOC entry 242 (class 1259 OID 39375)
-- Name: sites; Type: TABLE; Schema: land_admin; Owner: -
--

CREATE TABLE land_admin.sites (
    site_id text NOT NULL,
    site_name text NOT NULL,
    main_habitat_code text,
    site_category_code text,
    notes text,
    site_privacy_code text NOT NULL
);


--
-- TOC entry 6329 (class 0 OID 0)
-- Dependencies: 242
-- Name: TABLE sites; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TABLE land_admin.sites IS 'Master table of sites.';


--
-- TOC entry 6330 (class 0 OID 0)
-- Dependencies: 242
-- Name: COLUMN sites.site_id; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.sites.site_id IS 'Unique identifier for the site.';


--
-- TOC entry 6331 (class 0 OID 0)
-- Dependencies: 242
-- Name: COLUMN sites.site_name; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.sites.site_name IS 'Accepted name for the site.';


--
-- TOC entry 6332 (class 0 OID 0)
-- Dependencies: 242
-- Name: COLUMN sites.main_habitat_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.sites.main_habitat_code IS 'High level categorisation of the main habitat type across the site. Selected from a unique list of values.';


--
-- TOC entry 6333 (class 0 OID 0)
-- Dependencies: 242
-- Name: COLUMN sites.site_category_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.sites.site_category_code IS 'Categorisation of site from an operational point of view. Selected from a unique list of values.';


--
-- TOC entry 6334 (class 0 OID 0)
-- Dependencies: 242
-- Name: COLUMN sites.notes; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.sites.notes IS 'Free text notes about the site.';


--
-- TOC entry 243 (class 1259 OID 39380)
-- Name: vw_tenure_ownership; Type: VIEW; Schema: land_admin; Owner: -
--

CREATE VIEW land_admin.vw_tenure_ownership AS
 WITH own AS (
         SELECT ownership.ownership_uuid,
            ownership.parcel_uuid,
            ownership.freehold_title AS tenure_title,
            ownership.ownership_type_code AS tenure_type_code,
            dom_ownership_types.ownership_type AS tenure_type,
            ownership.landowner AS tenure_holder,
            ownership.ownership_date_start AS tenure_date_start,
            lead(ownership.ownership_date_start, 1) OVER (PARTITION BY ownership.parcel_uuid ORDER BY ownership.ownership_date_start NULLS FIRST) AS tenure_date_end,
            lag(ownership.landowner, 1) OVER (PARTITION BY ownership.parcel_uuid ORDER BY ownership.ownership_date_start NULLS FIRST) AS tenure_from,
            ownership.transfer_type_code,
            ownership.purchase_amount_gbp,
            ownership.updated_at
           FROM (land_admin.ownership
             LEFT JOIN domains.dom_ownership_types USING (ownership_type_code))
        )
 SELECT own.ownership_uuid,
    parcels.site_id,
    sites.site_name,
    own.parcel_uuid,
    parcels.parcel_alias,
    own.tenure_title,
    own.tenure_type_code,
    own.tenure_type,
    own.tenure_holder,
    own.tenure_date_start,
    own.tenure_date_end,
        CASE
            WHEN (own.tenure_date_start > CURRENT_DATE) THEN 'Forthcoming'::text
            WHEN (daterange(own.tenure_date_start, own.tenure_date_end) @> CURRENT_DATE) THEN 'Live'::text
            ELSE 'Ended'::text
        END AS tenure_status,
        CASE
            WHEN (own.tenure_date_start > CURRENT_DATE) THEN 'Prospective'::text
            WHEN (daterange(own.tenure_date_start, own.tenure_date_end) @> CURRENT_DATE) THEN 'Active'::text
            ELSE 'Historic'::text
        END AS tenure_phase,
    own.tenure_from,
    own.transfer_type_code,
    own.purchase_amount_gbp,
    GREATEST(parcels.updated_at, own.updated_at) AS updated_at
   FROM ((own
     JOIN land_admin.parcels USING (parcel_uuid))
     JOIN land_admin.sites USING (site_id));


--
-- TOC entry 6335 (class 0 OID 0)
-- Dependencies: 243
-- Name: VIEW vw_tenure_ownership; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON VIEW land_admin.vw_tenure_ownership IS 'Lookup view for QGIS tables.';


--
-- TOC entry 244 (class 1259 OID 39385)
-- Name: vw_tenure_occupancy; Type: VIEW; Schema: land_admin; Owner: -
--

CREATE VIEW land_admin.vw_tenure_occupancy AS
 WITH trust_occupancy_status AS (
         SELECT occupancy_1.occupancy_uuid,
                CASE
                    WHEN (occupancy_1.termination_date <= CURRENT_DATE) THEN 'Ended'::text
                    WHEN (occupancy_1.occupancy_date_start > CURRENT_DATE) THEN 'Forthcoming'::text
                    WHEN (COALESCE(occupancy_1.occupancy_date_end, CURRENT_DATE) >= CURRENT_DATE) THEN 'Live'::text
                    ELSE 'Expired'::text
                END AS tenure_status,
                CASE
                    WHEN (occupancy_1.termination_date <= CURRENT_DATE) THEN 'Historic'::text
                    WHEN (COALESCE(occupancy_1.occupancy_date_end, CURRENT_DATE) >= CURRENT_DATE) THEN
                    CASE
                        WHEN (occupancy_1.occupancy_type_code = 'U'::text) THEN 'Occupying'::text
                        ELSE 'Active'::text
                    END
                    WHEN (occupancy_1.renewal_type_code = 'PERIODIC'::text) THEN 'Periodic'::text
                    WHEN (occupancy_1.renewal_type_code = 'HOLD'::text) THEN 'Holdover'::text
                    ELSE 'Squat'::text
                END AS tenure_phase
           FROM land_admin.occupancy occupancy_1
        )
 SELECT occ.occupancy_uuid,
    own.site_id,
    own.site_name,
    own.parcel_uuid,
    own.parcel_alias,
    occ.leasehold_title AS tenure_title,
    occ.occupancy_type_code AS tenure_type_code,
    dom_occupancy_types.occupancy_type AS tenure_type,
    own.tenure_holder AS tenure_from,
    occ.occupant AS tenure_holder,
    trust_occupancy_status.tenure_status,
    trust_occupancy_status.tenure_phase,
    occ.occupancy_date_start AS tenure_date_start,
    occ.occupancy_date_end AS tenure_date_end,
    occ.termination_date AS tenure_termination_date,
    occ.renewal_type_code,
    dom_renewal_types.renewal_type,
    GREATEST(occ.updated_at, own.updated_at) AS updated_at
   FROM ((((land_admin.occupancy occ
     LEFT JOIN trust_occupancy_status USING (occupancy_uuid))
     LEFT JOIN land_admin.vw_tenure_ownership own USING (ownership_uuid))
     LEFT JOIN domains.dom_occupancy_types USING (occupancy_type_code))
     LEFT JOIN domains.dom_renewal_types USING (renewal_type_code));


--
-- TOC entry 6336 (class 0 OID 0)
-- Dependencies: 244
-- Name: VIEW vw_tenure_occupancy; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON VIEW land_admin.vw_tenure_occupancy IS 'Lookup view for QGIS tables.';


--
-- TOC entry 245 (class 1259 OID 39390)
-- Name: vw_tenure_all; Type: VIEW; Schema: land_admin; Owner: -
--

CREATE VIEW land_admin.vw_tenure_all AS
 SELECT vw_tenure_ownership.ownership_uuid AS tenure_uuid,
    vw_tenure_ownership.site_id,
    vw_tenure_ownership.site_name,
    vw_tenure_ownership.parcel_uuid,
    vw_tenure_ownership.parcel_alias,
    'ownership'::text AS tenure_layer,
    vw_tenure_ownership.tenure_title,
    vw_tenure_ownership.tenure_type_code,
    vw_tenure_ownership.tenure_type,
    vw_tenure_ownership.tenure_from,
    vw_tenure_ownership.tenure_holder,
    vw_tenure_ownership.tenure_status,
    vw_tenure_ownership.tenure_phase,
    vw_tenure_ownership.tenure_date_start,
    vw_tenure_ownership.tenure_date_end,
    vw_tenure_ownership.tenure_date_end AS tenure_termination_date,
    NULL::text AS renewal_type_code,
    NULL::text AS renewal_type,
    vw_tenure_ownership.updated_at
   FROM land_admin.vw_tenure_ownership
UNION ALL
 SELECT vw_tenure_occupancy.occupancy_uuid AS tenure_uuid,
    vw_tenure_occupancy.site_id,
    vw_tenure_occupancy.site_name,
    vw_tenure_occupancy.parcel_uuid,
    vw_tenure_occupancy.parcel_alias,
    'occupancy'::text AS tenure_layer,
    vw_tenure_occupancy.tenure_title,
    vw_tenure_occupancy.tenure_type_code,
    vw_tenure_occupancy.tenure_type,
    vw_tenure_occupancy.tenure_from,
    vw_tenure_occupancy.tenure_holder,
    vw_tenure_occupancy.tenure_status,
    vw_tenure_occupancy.tenure_phase,
    vw_tenure_occupancy.tenure_date_start,
    vw_tenure_occupancy.tenure_date_end,
    vw_tenure_occupancy.tenure_termination_date,
    vw_tenure_occupancy.renewal_type_code,
    vw_tenure_occupancy.renewal_type,
    vw_tenure_occupancy.updated_at
   FROM land_admin.vw_tenure_occupancy;


--
-- TOC entry 6337 (class 0 OID 0)
-- Dependencies: 245
-- Name: VIEW vw_tenure_all; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON VIEW land_admin.vw_tenure_all IS 'Amalgamated view of teh tenure history and current status. This is used for generating the export views.';


--
-- TOC entry 6338 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_uuid IS 'Unique identifier of either the `occupation_uuid` or `ownership_uuid` depending on the `tenure_layer` the record was retreived from.';


--
-- TOC entry 6339 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.site_id; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.site_id IS 'Site unique identifier.';


--
-- TOC entry 6340 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.site_name; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.site_name IS 'Name of the site.';


--
-- TOC entry 6341 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.parcel_uuid; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.parcel_uuid IS 'Parcel unique identifier.';


--
-- TOC entry 6342 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.parcel_alias; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.parcel_alias IS 'Local name of the parcel.';


--
-- TOC entry 6343 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_layer; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_layer IS 'States which table the information is primarily compiled from.';


--
-- TOC entry 6344 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_title; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_title IS 'Deed title number.';


--
-- TOC entry 6345 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_type_code IS 'Short code representing the tenure type. See `tenure_type` for code meaning.';


--
-- TOC entry 6346 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_type; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_type IS 'Agreement type under which the land is owned or occupied.';


--
-- TOC entry 6347 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_from; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_from IS 'Previous party holding the tenure (or in the case of occupation, the owner).';


--
-- TOC entry 6348 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_holder; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_holder IS 'Holder of the `tenure_type` between the `tenure_date_start` and `tenure_date_end`.';


--
-- TOC entry 6349 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_status; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_status IS 'Status of the tenure based on `CURRENT_DATE`';


--
-- TOC entry 6350 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_phase; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_phase IS 'Classification of the tenure.';


--
-- TOC entry 6351 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_date_start; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_date_start IS 'Start of agreed tenure.';


--
-- TOC entry 6352 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_date_end; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_date_end IS 'End of agreed tenure.';


--
-- TOC entry 6353 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.tenure_termination_date; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.tenure_termination_date IS 'Date of last involvement by the `tenure_holder` under the stated tenure agreement.';


--
-- TOC entry 6354 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.renewal_type_code; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.renewal_type_code IS 'Short code representing the occupancy renewal type. See `renewal_type` for code meaning.';


--
-- TOC entry 6355 (class 0 OID 0)
-- Dependencies: 245
-- Name: COLUMN vw_tenure_all.renewal_type; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON COLUMN land_admin.vw_tenure_all.renewal_type IS 'States the terms of what happens after the occupancy `tenure_date_end`.';


--
-- TOC entry 252 (class 1259 OID 39738)
-- Name: sites_export; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.sites_export AS
 WITH trust_site_involvement AS (
         SELECT parcels.site_id,
            min(vw_tenure_all.tenure_date_start) AS date_established
           FROM (land_admin.vw_tenure_all
             JOIN land_admin.parcels USING (parcel_uuid))
          WHERE (vw_tenure_all.tenure_holder = 'Trust'::text)
          GROUP BY parcels.site_id
        ), trust_site_geom AS (
         SELECT parcels.site_id,
            COALESCE(sum(utils.st_areaha(parcels.geom, true)) FILTER (WHERE ((tenure.tenure_layer = 'F'::text) AND (tenure.tenure_type_code <> 'U'::text))), (0)::numeric) AS freehold_areaha,
            COALESCE(sum(utils.st_areaha(parcels.geom, true)) FILTER (WHERE ((tenure.tenure_layer = 'L'::text) AND (tenure.tenure_type_code = 'LH'::text))), (0)::numeric) AS leased_areaha,
            COALESCE(sum(utils.st_areaha(parcels.geom, true)) FILTER (WHERE ((tenure.tenure_layer = 'L'::text) AND (tenure.tenure_type_code <> ALL (ARRAY['LH'::text, 'U'::text])))), (0)::numeric) AS management_agreement_areaha,
            COALESCE(sum(utils.st_areaha(parcels.geom, true)) FILTER (WHERE (tenure.tenure_type_code = 'U'::text)), (0)::numeric) AS unknown_tenure_areaha,
            round((((100)::numeric * COALESCE(sum(utils.st_areaha(parcels.geom)) FILTER (WHERE ((tenure.tenure_layer = 'F'::text) AND (tenure.tenure_type_code <> 'U'::text))), (0)::numeric)) / sum(utils.st_areaha(parcels.geom))), 2) AS pct_freehold,
            utils.st_areaha(postgis.st_union(parcels.geom), true) AS area_ha,
            count(parcels.parcel_uuid) AS n_land_parcels,
            max(tenure.updated_at) AS updated_at,
            (postgis.st_makevalid(postgis.st_union(parcels.geom)))::postgis.geometry(MultiPolygon,27700) AS geom
           FROM (land_admin.parcels
             JOIN land_admin.vw_tenure_all tenure USING (parcel_uuid))
          WHERE ((tenure.tenure_status = ANY (ARRAY['Live'::text, 'Expired'::text])) AND (tenure.tenure_holder = 'Trust'::text))
          GROUP BY parcels.site_id
        )
 SELECT sites.site_name,
    trust_site_geom.site_id,
    trust_site_geom.area_ha,
    trust_site_involvement.date_established,
    dom_main_habitats.main_habitat,
    dom_site_categories.site_category,
    dom_site_privacy.site_privacy,
    (trust_site_geom.updated_at)::date AS updated_at,
    CURRENT_DATE AS valid_at,
    (postgis.st_multi(trust_site_geom.geom))::postgis.geometry(MultiPolygon,27700) AS geom
   FROM (((((trust_site_geom
     LEFT JOIN trust_site_involvement USING (site_id))
     LEFT JOIN land_admin.sites USING (site_id))
     LEFT JOIN domains.dom_main_habitats USING (main_habitat_code))
     LEFT JOIN domains.dom_site_categories USING (site_category_code))
     LEFT JOIN domains.dom_site_privacy USING (site_privacy_code))
  ORDER BY sites.site_name;


--
-- TOC entry 246 (class 1259 OID 39400)
-- Name: tenure_export; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.tenure_export AS
 SELECT row_number() OVER () AS objectid,
    sites.site_id,
    sites.site_name,
    (parcels.parcel_uuid)::text AS parcel_id,
    tenure.tenure_title AS parcel_title,
    tenure.tenure_type,
    tenure.tenure_status,
    tenure.tenure_phase,
    tenure.tenure_date_start AS tenure_start_date,
    tenure.tenure_date_end AS tenure_end_date,
    dom_boundary_types.boundary_type,
    utils.st_areaha(parcels.geom, true) AS area_ha,
    (tenure.updated_at)::date AS updated_at,
    CURRENT_DATE AS valid_at,
    parcels.geom
   FROM (((land_admin.sites
     LEFT JOIN land_admin.parcels USING (site_id))
     LEFT JOIN land_admin.vw_tenure_all tenure USING (parcel_uuid))
     LEFT JOIN domains.dom_boundary_types USING (boundary_type_code))
  WHERE ((tenure.tenure_holder = 'Trust'::text) AND (tenure.tenure_status = ANY (ARRAY['Live'::text, 'Expired'::text])))
  ORDER BY sites.site_name, ((tenure.tenure_date_start)::timestamp without time zone) NULLS FIRST, tenure.tenure_date_end DESC NULLS LAST;


--
-- TOC entry 247 (class 1259 OID 39405)
-- Name: tenure_history_export; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW public.tenure_history_export AS
 WITH all_parcels AS (
         SELECT parcels.parcel_uuid,
            parcels.site_id,
            parcels.valid_from,
            parcels.valid_to,
            parcels.geom
           FROM land_admin.parcels
        UNION
         SELECT parcel_history.parcel_uuid,
            parcel_history.site_id,
            parcel_history.valid_from,
            parcel_history.valid_to,
            parcel_history.geom
           FROM land_admin.parcel_history
        )
 SELECT row_number() OVER () AS gid,
    p.site_id,
    p.parcel_uuid,
    t.tenure_type,
    t.tenure_holder,
    lower((daterange(p.valid_from, p.valid_to) * daterange(t.tenure_date_start,
        CASE
            WHEN (t.renewal_type_code <> ALL (ARRAY['HOLD'::text, 'PERIODIC'::text])) THEN t.tenure_date_end
            ELSE t.tenure_termination_date
        END))) AS s_date,
    upper((daterange(p.valid_from, p.valid_to) * daterange(t.tenure_date_start,
        CASE
            WHEN (t.renewal_type_code <> ALL (ARRAY['HOLD'::text, 'PERIODIC'::text])) THEN t.tenure_date_end
            ELSE t.tenure_termination_date
        END))) AS e_date,
    p.geom
   FROM (land_admin.vw_tenure_all t
     JOIN all_parcels p ON (((t.parcel_uuid = p.parcel_uuid) AND (daterange(p.valid_from, p.valid_to) && daterange(t.tenure_date_start,
        CASE
            WHEN (t.renewal_type_code <> ALL (ARRAY['HOLD'::text, 'PERIODIC'::text])) THEN t.tenure_date_end
            ELSE t.tenure_termination_date
        END)) AND (t.tenure_holder = 'Trust'::text))))
  ORDER BY p.site_id, p.parcel_uuid, p.valid_from;


--
-- TOC entry 248 (class 1259 OID 39410)
-- Name: layer_styles; Type: TABLE; Schema: qgis; Owner: -
--

CREATE TABLE qgis.layer_styles (
    id integer NOT NULL,
    f_table_catalog character varying,
    f_table_schema character varying,
    f_table_name character varying,
    f_geometry_column character varying,
    stylename text,
    styleqml xml,
    stylesld xml,
    useasdefault boolean,
    description text,
    owner character varying(63) DEFAULT CURRENT_USER,
    ui xml,
    update_time timestamp without time zone DEFAULT CURRENT_TIMESTAMP,
    type character varying
);


--
-- TOC entry 249 (class 1259 OID 39417)
-- Name: layer_styles_id_seq; Type: SEQUENCE; Schema: qgis; Owner: -
--

CREATE SEQUENCE qgis.layer_styles_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- TOC entry 6356 (class 0 OID 0)
-- Dependencies: 249
-- Name: layer_styles_id_seq; Type: SEQUENCE OWNED BY; Schema: qgis; Owner: -
--

ALTER SEQUENCE qgis.layer_styles_id_seq OWNED BY qgis.layer_styles.id;


--
-- TOC entry 250 (class 1259 OID 39418)
-- Name: qgis_projects; Type: TABLE; Schema: qgis; Owner: -
--

CREATE TABLE qgis.qgis_projects (
    name text NOT NULL,
    metadata jsonb,
    content bytea
);


--
-- TOC entry 251 (class 1259 OID 39423)
-- Name: db_size; Type: VIEW; Schema: utils; Owner: -
--

CREATE VIEW utils.db_size AS
 SELECT pg_size_pretty(pg_database_size(current_database())) AS pg_size_pretty;


--
-- TOC entry 6011 (class 2604 OID 39427)
-- Name: layer_styles id; Type: DEFAULT; Schema: qgis; Owner: -
--

ALTER TABLE ONLY qgis.layer_styles ALTER COLUMN id SET DEFAULT nextval('qgis.layer_styles_id_seq'::regclass);


--
-- TOC entry 6224 (class 0 OID 39293)
-- Dependencies: 230
-- Data for Name: dom_boundary_types; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_boundary_types VALUES ('IND', 'Indicative', 'This indicates that the boundary is indicative only and does not necessarily reflect the legally agreed boundary. This is a useful flag to allow data users to take care when referring to these boundaries.', 1);
INSERT INTO domains.dom_boundary_types VALUES ('PROP', 'Proposed', 'This indicates a potential new site where there is no legal agreement yet in place.', 2);
INSERT INTO domains.dom_boundary_types VALUES ('LEGAL', 'Legal', 'Indicates that the boundary matches, as far as possible, the legally agreed extents. All boundaries are only ever truly indicative and should never be assumed to be infallible but more confidence can be given to sites with this flag that the best efforts have been made to accurately denote the true extent.', 3);


--
-- TOC entry 6225 (class 0 OID 39298)
-- Dependencies: 231
-- Data for Name: dom_main_habitats; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_main_habitats VALUES ('WOOD', 'Woodland and Forest', 'If the site is mainly considered a woodland reserve or the predominate landcover type is woodland.', 1);
INSERT INTO domains.dom_main_habitats VALUES ('HEATH', 'Heathland and shrub', 'If the site is mainly considered a heathland reserve or the predominate landcover type is heathland.', 2);
INSERT INTO domains.dom_main_habitats VALUES ('SCRUB', 'Scrub', 'If the site is mainly considered to be scrubland reserve or the predominate landcover type is scrub.', 3);
INSERT INTO domains.dom_main_habitats VALUES ('COAST', 'Coastal', 'If the site is mainly considered a grassland reserve or the predominate landcover type is grassland.', 4);
INSERT INTO domains.dom_main_habitats VALUES ('GRASS', 'Grassland', 'If the site is mainly considered a grassland reserve or the predominate landcover type is one of the grassland habitat types.', 5);
INSERT INTO domains.dom_main_habitats VALUES ('ROCK', 'Rocky', 'If the site is mainly considered rocky or sparesely vegetated i.e quarry, scree slopes, inland sand dunes.', 6);
INSERT INTO domains.dom_main_habitats VALUES ('ARBL', 'Arable and Horticulture', 'If the site is mainly considered an arable or horticultural site i.e community garden, orchard or arable farm.', 7);
INSERT INTO domains.dom_main_habitats VALUES ('WET', 'Wetland', 'If the site is mainly considered a wetland reserve or the predominate landcover type is wetland.', 8);
INSERT INTO domains.dom_main_habitats VALUES ('NA', 'No main habitat', 'If it is not possible to determine and assign a main habitat type this category can be used. It is also used for the administrative category.', 9);


--
-- TOC entry 6226 (class 0 OID 39303)
-- Dependencies: 232
-- Data for Name: dom_occupancy_types; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_occupancy_types VALUES ('LH', 'Leasehold', 'Leasehold is a form of property tenure where the leaseholder (tenant) holds the right to occupy and use the property for a specified period, as outlined in a lease agreement, which can range from a few years to several decades. The leaseholder does not own the land; ownership remains with the freeholder (landlord). Leaseholders typically pay rent to the freeholder and may be responsible for maintenance and other costs. At the end of the lease term, ownership and possession of the property revert to the freeholder.', 1);
INSERT INTO domains.dom_occupancy_types VALUES ('MA', 'Management Agreement', 'A Management Agreement is a legal contract between a property owner and a management company or individual responsible for overseeing the operation and maintenance of the property. This agreement outlines the responsibilities of the manager, including property upkeep, financial management, tenant relations, and compliance with regulations. It typically specifies the duration of the agreement, fees, and performance expectations, allowing property owners to ensure their assets are effectively managed without directly handling day-to-day operations.', 2);
INSERT INTO domains.dom_occupancy_types VALUES ('LI', 'Licence', 'A Licence is a legal agreement that permits an individual or entity to use a property owned by another party for a specific purpose, without transferring any ownership rights. Unlike a lease, a licence is usually more flexible and can be granted for a short term or for a specific activity (e.g., holding an event, accessing land for recreation). Licences are generally revocable and do not create an interest in the property, meaning the licensor retains control and can terminate the agreement as stipulated.', 3);
INSERT INTO domains.dom_occupancy_types VALUES ('FBT', 'Farm Business Tenancy', 'A Farm Business Tenancy (FBT) is a specific type of agricultural tenancy in the UK that allows a tenant to farm land for a defined period while adhering to the terms of the tenancy agreement. FBTs typically grant tenants the right to cultivate crops, keep livestock, and engage in agricultural activities. They are governed by specific legislation, providing tenants with certain protections, such as security of tenure and the ability to claim compensation for improvements made to the property. This arrangement supports sustainable farming practices while enabling landowners to retain ownership of their land.', 4);
INSERT INTO domains.dom_occupancy_types VALUES ('U', 'Unknown occupation', 'Unknown tenure types are where the land is being actively managed but there is no agreement in place or no documentation can be found to establish the type of tenure that is in place. These parcels need to be resolved as a priority and so it is very useful to flag instances where this occurs.', 5);


--
-- TOC entry 6227 (class 0 OID 39308)
-- Dependencies: 233
-- Data for Name: dom_ownership_types; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_ownership_types VALUES ('FH', 'Freehold', 'Freehold is a type of property ownership that grants the owner full control and rights over the land and any structures on it for an indefinite period. This tenure type is the most complete form of ownership.', 1);
INSERT INTO domains.dom_ownership_types VALUES ('U', 'Unknown ownership', 'Unknown tenure types are where the land is being actively managed but there is no agreement in place or no documentation can be found to establish the type of tenure that is in place. These parcels need to be resolved as a priority and so it is very useful to flag instances where this occurs.', 2);


--
-- TOC entry 6228 (class 0 OID 39313)
-- Dependencies: 234
-- Data for Name: dom_renewal_types; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_renewal_types VALUES ('OUT_1954', 'Contracted out of Landlord and Tanants Act 1954', 'Excluded from the provisions of sections 24 to 28 of the Landlord and Tenants Act 1954. If this is the case then then there is no holding over of the lease after the expiry and a new lease must be negotiated.', 2);
INSERT INTO domains.dom_renewal_types VALUES ('HOLD', 'Holdover', 'Tenancy remains after the expiration of the lease so long as the tenant continues payments to the landlord.', 1);
INSERT INTO domains.dom_renewal_types VALUES ('PERIODIC', 'Periodic', 'Tenancies that do not have an end date specified.', 3);
INSERT INTO domains.dom_renewal_types VALUES ('NONE', 'None', 'Marks that lease terminates at the end of the term. This is used where a contract specifically states the contract will end ona specific date and is also used for archiving tenancies that have expired.', 4);


--
-- TOC entry 6229 (class 0 OID 39318)
-- Dependencies: 235
-- Data for Name: dom_site_categories; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_site_categories VALUES ('NR', 'Nature Reserve', 'The default category. A protected area established to conserve biodiversity, safeguard habitats, and promote ecological integrity. These sites are often designated for the preservation of native flora and fauna, offering a sanctuary for wildlife. These sites welcome visitors where they can engage in guided walks and wildlife observation, all while fostering a deeper appreciation for our natural ecosystems.', 1);
INSERT INTO domains.dom_site_categories VALUES ('EX', 'Explorer Site', 'A site designated for encouraging active engagement and exploration of natural landscapes. Explorer Sites serve as gateways for individuals to discover and learn about nature, promoting conservation awareness through hands-on experiences. These sites will usually have a visitor centre or other similar visitor enagement facilities. They may also be called ''gateway'' sites.', 2);
INSERT INTO domains.dom_site_categories VALUES ('WLD', 'Wild', 'This category encompasses areas that are largely untouched by human activity. These sites promote minimal human interference, allowing nature to thrive without significant management. Wild areas are essential for preserving biodiversity and providing habitat for various species. There may be little to no visitor infrastructure and no ongoing mangement.', 3);
INSERT INTO domains.dom_site_categories VALUES ('ARC', 'Arc Site', 'This is a conservation area specifically designed to serve as a refuge for vulnerable species and a hub for ecological restoration efforts. These sites may be strategically selected to connect fragmented habitats and enhance species movement across landscapes. The Arc approach emphasizes resilience and adaptability, focusing on both the preservation of existing biodiversity and the restoration of degraded habitats. the sensitivity of these sites often mean they will not be open to the public.', 4);
INSERT INTO domains.dom_site_categories VALUES ('DON', 'Donor site', 'A conservation area where biodiversity is actively cultivated and managed to provide resources for other conservation initiatives. These sites often focus on sustainable practices that promote the growth of native plants and animals, serving as a source for rewilding efforts and restoration projects elsewhere. Donor Sites contribute to regional biodiversity by supplying seeds, cuttings, or animal populations to help re-establish ecological balance in degraded areas.', 5);
INSERT INTO domains.dom_site_categories VALUES ('FRM', 'Farm', 'In the context of nature conservation this is an active agricultural site that integrates sustainable practices with biodiversity conservation. They prioritise environemntally methods, such as organic farming, permaculture, and agroecology, to enhance soil health and support wildlife habitats. These sites often contribute to local ecosystems while providing food and resources for surrounding communities. Educational programs often highlight the connection between agriculture and conservation. This is not a category for old farms that may now be undergoing rewilding with no active farming taking place.', 6);
INSERT INTO domains.dom_site_categories VALUES ('LM', 'Livestock Management', 'These sites focus on sustainable practices for raising animals while minimizing environmental impact. They are not fully operational farms but may be grassland sites where grazing stock are kept between rotations onto other sites. Generally speaking they may not be open to the public as there is little to no ecological interest and they are operational sites where visitors may be at risk.', 7);
INSERT INTO domains.dom_site_categories VALUES ('AD', 'Administrative', 'These sites relate to administrative functions of the organisation. It could be offices, tools stores, shops or even education centres', 8);


--
-- TOC entry 6230 (class 0 OID 39323)
-- Dependencies: 236
-- Data for Name: dom_site_privacy; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_site_privacy VALUES ('3', 'Internal', 'Must not be shared outside of The Trust without proper authorisation. ', 3);
INSERT INTO domains.dom_site_privacy VALUES ('2', 'Partnership', 'Some sites have delicate relationships with land owners. These site boundaries and associated fields will be shared with other Trusts on the shared platform but will not be shared further witout obtaining permission. Derviative data can be used by RSWT or other Trusts such as aggregation of total areas. These sites won''t be publicised in the guidebook or on the website.', 2);
INSERT INTO domains.dom_site_privacy VALUES ('1', 'Public', 'In general there are no restrictions on sharing the boundary publicly. There may be specific restrictions on which other fields are shared but these have been agreed on by the data owner. These sites can go in the guidebook and on the website.', 1);


--
-- TOC entry 6231 (class 0 OID 39333)
-- Dependencies: 237
-- Data for Name: dom_transfer_types; Type: TABLE DATA; Schema: domains; Owner: -
--

INSERT INTO domains.dom_transfer_types VALUES ('SALE', 'Sale', 'Land given in exchange for money.', 1);
INSERT INTO domains.dom_transfer_types VALUES ('GIFT', 'Gift', 'Land given freely.', 2);
INSERT INTO domains.dom_transfer_types VALUES ('EXC', 'Exchange', 'Land exchanged for another piece of land.', 3);
INSERT INTO domains.dom_transfer_types VALUES ('ADV', 'Adverse posession', 'Legally claimed ownership of the land due to extended occupation.', 4);
INSERT INTO domains.dom_transfer_types VALUES ('U', 'Unknown', 'Unknow how the land was acquired.', 5);


--
-- TOC entry 6232 (class 0 OID 39338)
-- Dependencies: 238
-- Data for Name: occupancy; Type: TABLE DATA; Schema: land_admin; Owner: -
--



--
-- TOC entry 6233 (class 0 OID 39348)
-- Dependencies: 239
-- Data for Name: ownership; Type: TABLE DATA; Schema: land_admin; Owner: -
--



--
-- TOC entry 6234 (class 0 OID 39357)
-- Dependencies: 240
-- Data for Name: parcel_history; Type: TABLE DATA; Schema: land_admin; Owner: -
--



--
-- TOC entry 6235 (class 0 OID 39366)
-- Dependencies: 241
-- Data for Name: parcels; Type: TABLE DATA; Schema: land_admin; Owner: -
--



--
-- TOC entry 6236 (class 0 OID 39375)
-- Dependencies: 242
-- Data for Name: sites; Type: TABLE DATA; Schema: land_admin; Owner: -
--



--
-- TOC entry 5998 (class 0 OID 38510)
-- Dependencies: 226
-- Data for Name: spatial_ref_sys; Type: TABLE DATA; Schema: postgis; Owner: -
--



--
-- TOC entry 6237 (class 0 OID 39410)
-- Dependencies: 248
-- Data for Name: layer_styles; Type: TABLE DATA; Schema: qgis; Owner: -
--

INSERT INTO qgis.layer_styles VALUES (5, 'land_terrier', 'land_admin', 'tenure', NULL, 'tenure', '<!DOCTYPE qgis PUBLIC ''http://mrcc.com/qgis.dtd'' ''SYSTEM''>
<qgis minScale="1e+08" hasScaleBasedVisibilityFlag="0" maxScale="0" styleCategories="AllStyleCategories" version="3.34.8-Prizren" readOnly="0">
 <flags>
  <Identifiable>1</Identifiable>
  <Removable>1</Removable>
  <Searchable>1</Searchable>
  <Private>0</Private>
 </flags>
 <temporal durationUnit="min" fixedDuration="0" enabled="0" mode="2" limitMode="0" startField="" endField="" durationField="" accumulate="0" endExpression="" startExpression="">
  <fixedRange>
   <start></start>
   <end></end>
  </fixedRange>
 </temporal>
 <elevation zscale="1" showMarkerSymbolInSurfacePlots="0" extrusionEnabled="0" type="IndividualFeatures" respectLayerSymbol="1" binding="Centroid" clamping="Terrain" extrusion="0" zoffset="0" symbology="Line">
  <data-defined-properties>
   <Option type="Map">
    <Option value="" type="QString" name="name"/>
    <Option name="properties"/>
    <Option value="collection" type="QString" name="type"/>
   </Option>
  </data-defined-properties>
  <profileLineSymbol>
   <symbol alpha="1" type="line" force_rhr="0" is_animated="0" clip_to_extent="1" frame_rate="10" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" locked="0" id="{f684e689-10c3-4fbc-ae8e-d905f06d57af}" pass="0" class="SimpleLine">
     <Option type="Map">
      <Option value="0" type="QString" name="align_dash_pattern"/>
      <Option value="square" type="QString" name="capstyle"/>
      <Option value="5;2" type="QString" name="customdash"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="customdash_map_unit_scale"/>
      <Option value="MM" type="QString" name="customdash_unit"/>
      <Option value="0" type="QString" name="dash_pattern_offset"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="dash_pattern_offset_map_unit_scale"/>
      <Option value="MM" type="QString" name="dash_pattern_offset_unit"/>
      <Option value="0" type="QString" name="draw_inside_polygon"/>
      <Option value="bevel" type="QString" name="joinstyle"/>
      <Option value="114,155,111,255" type="QString" name="line_color"/>
      <Option value="solid" type="QString" name="line_style"/>
      <Option value="0.6" type="QString" name="line_width"/>
      <Option value="MM" type="QString" name="line_width_unit"/>
      <Option value="0" type="QString" name="offset"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="offset_map_unit_scale"/>
      <Option value="MM" type="QString" name="offset_unit"/>
      <Option value="0" type="QString" name="ring_filter"/>
      <Option value="0" type="QString" name="trim_distance_end"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="trim_distance_end_map_unit_scale"/>
      <Option value="MM" type="QString" name="trim_distance_end_unit"/>
      <Option value="0" type="QString" name="trim_distance_start"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="trim_distance_start_map_unit_scale"/>
      <Option value="MM" type="QString" name="trim_distance_start_unit"/>
      <Option value="0" type="QString" name="tweak_dash_pattern_on_corners"/>
      <Option value="0" type="QString" name="use_custom_dash"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="width_map_unit_scale"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option value="" type="QString" name="name"/>
       <Option name="properties"/>
       <Option value="collection" type="QString" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileLineSymbol>
  <profileFillSymbol>
   <symbol alpha="1" type="fill" force_rhr="0" is_animated="0" clip_to_extent="1" frame_rate="10" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" locked="0" id="{df958b36-d5a2-47d0-a49b-c5ac40b040d2}" pass="0" class="SimpleFill">
     <Option type="Map">
      <Option value="3x:0,0,0,0,0,0" type="QString" name="border_width_map_unit_scale"/>
      <Option value="114,155,111,255" type="QString" name="color"/>
      <Option value="bevel" type="QString" name="joinstyle"/>
      <Option value="0,0" type="QString" name="offset"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="offset_map_unit_scale"/>
      <Option value="MM" type="QString" name="offset_unit"/>
      <Option value="81,111,79,255" type="QString" name="outline_color"/>
      <Option value="solid" type="QString" name="outline_style"/>
      <Option value="0.2" type="QString" name="outline_width"/>
      <Option value="MM" type="QString" name="outline_width_unit"/>
      <Option value="solid" type="QString" name="style"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option value="" type="QString" name="name"/>
       <Option name="properties"/>
       <Option value="collection" type="QString" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileFillSymbol>
  <profileMarkerSymbol>
   <symbol alpha="1" type="marker" force_rhr="0" is_animated="0" clip_to_extent="1" frame_rate="10" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option value="" type="QString" name="name"/>
      <Option name="properties"/>
      <Option value="collection" type="QString" name="type"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" locked="0" id="{f7f184de-94b4-41a5-b553-7413ab20476d}" pass="0" class="SimpleMarker">
     <Option type="Map">
      <Option value="0" type="QString" name="angle"/>
      <Option value="square" type="QString" name="cap_style"/>
      <Option value="114,155,111,255" type="QString" name="color"/>
      <Option value="1" type="QString" name="horizontal_anchor_point"/>
      <Option value="bevel" type="QString" name="joinstyle"/>
      <Option value="diamond" type="QString" name="name"/>
      <Option value="0,0" type="QString" name="offset"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="offset_map_unit_scale"/>
      <Option value="MM" type="QString" name="offset_unit"/>
      <Option value="81,111,79,255" type="QString" name="outline_color"/>
      <Option value="solid" type="QString" name="outline_style"/>
      <Option value="0.2" type="QString" name="outline_width"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="outline_width_map_unit_scale"/>
      <Option value="MM" type="QString" name="outline_width_unit"/>
      <Option value="diameter" type="QString" name="scale_method"/>
      <Option value="3" type="QString" name="size"/>
      <Option value="3x:0,0,0,0,0,0" type="QString" name="size_map_unit_scale"/>
      <Option value="MM" type="QString" name="size_unit"/>
      <Option value="1" type="QString" name="vertical_anchor_point"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option value="" type="QString" name="name"/>
       <Option name="properties"/>
       <Option value="collection" type="QString" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileMarkerSymbol>
 </elevation>
 <customproperties>
  <Option type="Map">
   <Option value="offline" type="QString" name="QFieldSync/action"/>
   <Option value="{}" type="QString" name="QFieldSync/attachment_naming"/>
   <Option value="offline" type="QString" name="QFieldSync/cloud_action"/>
   <Option value="" type="QString" name="QFieldSync/geometry_locked_expression"/>
   <Option value="{}" type="QString" name="QFieldSync/photo_naming"/>
   <Option value="{}" type="QString" name="QFieldSync/relationship_maximum_visible"/>
   <Option value="30" type="int" name="QFieldSync/tracking_distance_requirement_minimum_meters"/>
   <Option value="1" type="int" name="QFieldSync/tracking_erroneous_distance_safeguard_maximum_meters"/>
   <Option value="0" type="int" name="QFieldSync/tracking_measurement_type"/>
   <Option value="30" type="int" name="QFieldSync/tracking_time_requirement_interval_seconds"/>
   <Option value="0" type="int" name="QFieldSync/value_map_button_interface_threshold"/>
   <Option type="List" name="dualview/previewExpressions">
    <Option value="COALESCE(format_date(tenure_date_start, ''yyyy:''), ''Unknown'') || COALESCE( '' '' || &quot;tenure_type&quot;, ''Unknown'' ) || coalesce('' '' || tenure_holder, ''Unknown'')" type="QString"/>
   </Option>
   <Option value="0" type="int" name="embeddedWidgets/count"/>
   <Option name="variableNames"/>
   <Option name="variableValues"/>
  </Option>
 </customproperties>
 <geometryOptions geometryPrecision="0" removeDuplicateNodes="0">
  <activeChecks/>
  <checkConfiguration/>
 </geometryOptions>
 <legend type="default-vector" showLabelLegend="0"/>
 <referencedLayers>
  <relation dataSource="dbname=''ywtgis_demo'' host=192.168.0.117 port=5432 key=''parcel_id'' srid=27700 type=MultiPolygon checkPrimaryKeyUnicity=''1'' table=&quot;land_admin&quot;.&quot;parcels&quot; (geom)" referencedLayer="parcels_c51f3997_8de2_4c43_b598_99f9e6534a61" strength="Association" providerKey="postgres" id="tenure_9a790ae4_c8d2_4d5c_afb5_3ab7ba90c7c5_parcel_id_parcels_c51f3997_8de2_4c43_b598_99f9e6534a61_parcel_id" referencingLayer="tenure_9a790ae4_c8d2_4d5c_afb5_3ab7ba90c7c5" layerId="parcels_c51f3997_8de2_4c43_b598_99f9e6534a61" name="tenure_parcel_id_fkey" layerName="parcels">
   <fieldRef referencingField="parcel_id" referencedField="parcel_id"/>
  </relation>
 </referencedLayers>
 <fieldConfiguration>
  <field configurationFlags="NoFlag" name="tenure_id">
   <editWidget type="Hidden">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="parcel_id">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="tenure_type">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="tenure_holder">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="tenure_registration">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="tenure_date_start">
   <editWidget type="DateTime">
    <config>
     <Option type="Map">
      <Option value="true" type="bool" name="allow_null"/>
      <Option value="true" type="bool" name="calendar_popup"/>
      <Option value="dd/MM/yyyy" type="QString" name="display_format"/>
      <Option value="yyyy-MM-dd" type="QString" name="field_format"/>
      <Option value="false" type="bool" name="field_format_overwrite"/>
      <Option value="false" type="bool" name="field_iso_format"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="tenure_date_end">
   <editWidget type="DateTime">
    <config>
     <Option type="Map">
      <Option value="true" type="bool" name="allow_null"/>
      <Option value="true" type="bool" name="calendar_popup"/>
      <Option value="dd/MM/yyyy" type="QString" name="display_format"/>
      <Option value="yyyy-MM-dd" type="QString" name="field_format"/>
      <Option value="false" type="bool" name="field_format_overwrite"/>
      <Option value="false" type="bool" name="field_iso_format"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="notes">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="true" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="date_modified">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="modified_by">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option value="false" type="bool" name="IsMultiline"/>
      <Option value="false" type="bool" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
 </fieldConfiguration>
 <aliases>
  <alias index="0" field="tenure_id" name=""/>
  <alias index="1" field="parcel_id" name=""/>
  <alias index="2" field="tenure_type" name=""/>
  <alias index="3" field="tenure_holder" name=""/>
  <alias index="4" field="tenure_registration" name=""/>
  <alias index="5" field="tenure_date_start" name=""/>
  <alias index="6" field="tenure_date_end" name=""/>
  <alias index="7" field="notes" name=""/>
  <alias index="8" field="date_modified" name=""/>
  <alias index="9" field="modified_by" name=""/>
 </aliases>
 <splitPolicies>
  <policy field="tenure_id" policy="DefaultValue"/>
  <policy field="parcel_id" policy="DefaultValue"/>
  <policy field="tenure_type" policy="DefaultValue"/>
  <policy field="tenure_holder" policy="DefaultValue"/>
  <policy field="tenure_registration" policy="DefaultValue"/>
  <policy field="tenure_date_start" policy="DefaultValue"/>
  <policy field="tenure_date_end" policy="DefaultValue"/>
  <policy field="notes" policy="DefaultValue"/>
  <policy field="date_modified" policy="DefaultValue"/>
  <policy field="modified_by" policy="DefaultValue"/>
 </splitPolicies>
 <defaults>
  <default applyOnUpdate="0" expression="" field="tenure_id"/>
  <default applyOnUpdate="0" expression="" field="parcel_id"/>
  <default applyOnUpdate="0" expression="" field="tenure_type"/>
  <default applyOnUpdate="0" expression="" field="tenure_holder"/>
  <default applyOnUpdate="0" expression="" field="tenure_registration"/>
  <default applyOnUpdate="0" expression="" field="tenure_date_start"/>
  <default applyOnUpdate="0" expression="" field="tenure_date_end"/>
  <default applyOnUpdate="0" expression="" field="notes"/>
  <default applyOnUpdate="0" expression="" field="date_modified"/>
  <default applyOnUpdate="0" expression="" field="modified_by"/>
 </defaults>
 <constraints>
  <constraint notnull_strength="1" exp_strength="0" unique_strength="1" field="tenure_id" constraints="3"/>
  <constraint notnull_strength="1" exp_strength="0" unique_strength="0" field="parcel_id" constraints="1"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="tenure_type" constraints="0"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="tenure_holder" constraints="0"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="tenure_registration" constraints="0"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="tenure_date_start" constraints="0"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="tenure_date_end" constraints="0"/>
  <constraint notnull_strength="0" exp_strength="0" unique_strength="0" field="notes" constraints="0"/>
  <constraint notnull_strength="2" exp_strength="0" unique_strength="0" field="date_modified" constraints="1"/>
  <constraint notnull_strength="2" exp_strength="0" unique_strength="0" field="modified_by" constraints="1"/>
 </constraints>
 <constraintExpressions>
  <constraint exp="" desc="" field="tenure_id"/>
  <constraint exp="" desc="" field="parcel_id"/>
  <constraint exp="" desc="" field="tenure_type"/>
  <constraint exp="" desc="" field="tenure_holder"/>
  <constraint exp="" desc="" field="tenure_registration"/>
  <constraint exp="" desc="" field="tenure_date_start"/>
  <constraint exp="" desc="" field="tenure_date_end"/>
  <constraint exp="" desc="" field="notes"/>
  <constraint exp="" desc="" field="date_modified"/>
  <constraint exp="" desc="" field="modified_by"/>
 </constraintExpressions>
 <expressionfields/>
 <attributeactions>
  <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
 </attributeactions>
 <attributetableconfig actionWidgetStyle="dropDown" sortOrder="0" sortExpression="&quot;tenure_date_start&quot;">
  <columns>
   <column type="field" width="-1" name="tenure_id" hidden="0"/>
   <column type="field" width="-1" name="parcel_id" hidden="0"/>
   <column type="field" width="-1" name="tenure_type" hidden="0"/>
   <column type="field" width="-1" name="tenure_holder" hidden="0"/>
   <column type="field" width="-1" name="tenure_registration" hidden="0"/>
   <column type="field" width="-1" name="tenure_date_start" hidden="0"/>
   <column type="field" width="-1" name="tenure_date_end" hidden="0"/>
   <column type="field" width="-1" name="notes" hidden="0"/>
   <column type="field" width="-1" name="date_modified" hidden="0"/>
   <column type="field" width="-1" name="modified_by" hidden="0"/>
   <column type="actions" width="-1" hidden="1"/>
  </columns>
 </attributetableconfig>
 <conditionalstyles>
  <rowstyles/>
  <fieldstyles/>
 </conditionalstyles>
 <storedexpressions/>
 <editform tolerant="1"></editform>
 <editforminit/>
 <editforminitcodesource>0</editforminitcodesource>
 <editforminitfilepath></editforminitfilepath>
 <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
    geom = feature.geometry()
    control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
 <featformsuppress>0</featformsuppress>
 <editorlayout>tablayout</editorlayout>
 <attributeEditorForm>
  <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
   <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
  </labelStyle>
  <attributeEditorContainer collapsedExpressionEnabled="0" visibilityExpressionEnabled="0" collapsed="0" horizontalStretch="0" verticalStretch="0" groupBox="0" type="Tab" visibilityExpression="" collapsedExpression="" columnCount="1" showLabel="1" name="History">
   <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
    <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
   </labelStyle>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="2" showLabel="1" name="tenure_type">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="3" showLabel="1" name="tenure_holder">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="4" showLabel="1" name="tenure_registration">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="5" showLabel="1" name="tenure_date_start">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="6" showLabel="1" name="tenure_date_end">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="7" showLabel="1" name="notes">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
  </attributeEditorContainer>
  <attributeEditorContainer collapsedExpressionEnabled="0" visibilityExpressionEnabled="0" collapsed="0" horizontalStretch="0" verticalStretch="0" groupBox="0" type="Tab" visibilityExpression="" collapsedExpression="" columnCount="1" showLabel="1" name="Metadata">
   <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
    <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
   </labelStyle>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="8" showLabel="1" name="date_modified">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField horizontalStretch="0" verticalStretch="0" index="9" showLabel="1" name="modified_by">
    <labelStyle overrideLabelColor="0" overrideLabelFont="0" labelColor="0,0,0,255">
     <labelFont style="" underline="0" strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0"/>
    </labelStyle>
   </attributeEditorField>
  </attributeEditorContainer>
 </attributeEditorForm>
 <editable>
  <field editable="0" name="date_modified"/>
  <field editable="0" name="modified_by"/>
  <field editable="1" name="notes"/>
  <field editable="0" name="parcel_id"/>
  <field editable="1" name="tenure_date_end"/>
  <field editable="1" name="tenure_date_start"/>
  <field editable="1" name="tenure_holder"/>
  <field editable="1" name="tenure_id"/>
  <field editable="1" name="tenure_registration"/>
  <field editable="1" name="tenure_type"/>
 </editable>
 <labelOnTop>
  <field labelOnTop="0" name="date_modified"/>
  <field labelOnTop="0" name="modified_by"/>
  <field labelOnTop="0" name="notes"/>
  <field labelOnTop="0" name="parcel_id"/>
  <field labelOnTop="0" name="tenure_date_end"/>
  <field labelOnTop="0" name="tenure_date_start"/>
  <field labelOnTop="0" name="tenure_holder"/>
  <field labelOnTop="0" name="tenure_id"/>
  <field labelOnTop="0" name="tenure_registration"/>
  <field labelOnTop="0" name="tenure_type"/>
 </labelOnTop>
 <reuseLastValue>
  <field reuseLastValue="0" name="date_modified"/>
  <field reuseLastValue="0" name="modified_by"/>
  <field reuseLastValue="0" name="notes"/>
  <field reuseLastValue="0" name="parcel_id"/>
  <field reuseLastValue="0" name="tenure_date_end"/>
  <field reuseLastValue="0" name="tenure_date_start"/>
  <field reuseLastValue="0" name="tenure_holder"/>
  <field reuseLastValue="0" name="tenure_id"/>
  <field reuseLastValue="0" name="tenure_registration"/>
  <field reuseLastValue="0" name="tenure_type"/>
 </reuseLastValue>
 <dataDefinedFieldProperties/>
 <widgets/>
 <previewExpression>COALESCE(format_date(tenure_date_start, ''yyyy:''), ''Unknown'') || COALESCE( '' '' || "tenure_type", ''Unknown'' ) || coalesce('' '' || tenure_holder, ''Unknown'')</previewExpression>
 <mapTip enabled="1"></mapTip>
 <layerGeometryType>4</layerGeometryType>
</qgis>
', '<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" version="1.1.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
 <NamedLayer/>
</StyledLayerDescriptor>
', true, 'Fri Sep 20 19:14:15 2024', 'dominic.hinchley', NULL, '2024-09-20 19:14:15.602952', 'Unknown geometry');
INSERT INTO qgis.layer_styles VALUES (1, 'land_terrier', 'land_admin', 'sites', 'geom', 'sites', '<!DOCTYPE qgis PUBLIC ''http://mrcc.com/qgis.dtd'' ''SYSTEM''>
<qgis simplifyLocal="1" simplifyDrawingTol="1" simplifyMaxScale="1" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories" labelsEnabled="1" readOnly="0" minScale="100000000" simplifyDrawingHints="1" version="3.34.8-Prizren" simplifyAlgorithm="0" maxScale="0" symbologyReferenceScale="-1">
 <flags>
  <Identifiable>1</Identifiable>
  <Removable>1</Removable>
  <Searchable>1</Searchable>
  <Private>0</Private>
 </flags>
 <temporal endField="" startField="" limitMode="0" startExpression="" durationField="" endExpression="" accumulate="0" fixedDuration="0" enabled="0" durationUnit="min" mode="0">
  <fixedRange>
   <start></start>
   <end></end>
  </fixedRange>
 </temporal>
 <elevation extrusion="0" type="IndividualFeatures" binding="Centroid" zscale="1" zoffset="0" showMarkerSymbolInSurfacePlots="0" symbology="Line" clamping="Terrain" respectLayerSymbol="1" extrusionEnabled="0">
  <data-defined-properties>
   <Option type="Map">
    <Option type="QString" value="" name="name"/>
    <Option name="properties"/>
    <Option type="QString" value="collection" name="type"/>
   </Option>
  </data-defined-properties>
  <profileLineSymbol>
   <symbol type="line" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </data_defined_properties>
    <layer id="{0fd26abf-9fe8-4e2e-824b-dad82e3470fd}" class="SimpleLine" locked="0" pass="0" enabled="1">
     <Option type="Map">
      <Option type="QString" value="0" name="align_dash_pattern"/>
      <Option type="QString" value="square" name="capstyle"/>
      <Option type="QString" value="5;2" name="customdash"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="customdash_map_unit_scale"/>
      <Option type="QString" value="MM" name="customdash_unit"/>
      <Option type="QString" value="0" name="dash_pattern_offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="dash_pattern_offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="dash_pattern_offset_unit"/>
      <Option type="QString" value="0" name="draw_inside_polygon"/>
      <Option type="QString" value="bevel" name="joinstyle"/>
      <Option type="QString" value="231,113,72,255" name="line_color"/>
      <Option type="QString" value="solid" name="line_style"/>
      <Option type="QString" value="0.6" name="line_width"/>
      <Option type="QString" value="MM" name="line_width_unit"/>
      <Option type="QString" value="0" name="offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="offset_unit"/>
      <Option type="QString" value="0" name="ring_filter"/>
      <Option type="QString" value="0" name="trim_distance_end"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_end_map_unit_scale"/>
      <Option type="QString" value="MM" name="trim_distance_end_unit"/>
      <Option type="QString" value="0" name="trim_distance_start"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_start_map_unit_scale"/>
      <Option type="QString" value="MM" name="trim_distance_start_unit"/>
      <Option type="QString" value="0" name="tweak_dash_pattern_on_corners"/>
      <Option type="QString" value="0" name="use_custom_dash"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="width_map_unit_scale"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileLineSymbol>
  <profileFillSymbol>
   <symbol type="fill" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </data_defined_properties>
    <layer id="{4fe2e85f-5f02-4285-b000-df7d2d40b88d}" class="SimpleFill" locked="0" pass="0" enabled="1">
     <Option type="Map">
      <Option type="QString" value="3x:0,0,0,0,0,0" name="border_width_map_unit_scale"/>
      <Option type="QString" value="231,113,72,255" name="color"/>
      <Option type="QString" value="bevel" name="joinstyle"/>
      <Option type="QString" value="0,0" name="offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="offset_unit"/>
      <Option type="QString" value="165,81,51,255" name="outline_color"/>
      <Option type="QString" value="solid" name="outline_style"/>
      <Option type="QString" value="0.2" name="outline_width"/>
      <Option type="QString" value="MM" name="outline_width_unit"/>
      <Option type="QString" value="solid" name="style"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileFillSymbol>
  <profileMarkerSymbol>
   <symbol type="marker" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </data_defined_properties>
    <layer id="{8b8d1f6d-3840-42e0-8bff-abed27b75415}" class="SimpleMarker" locked="0" pass="0" enabled="1">
     <Option type="Map">
      <Option type="QString" value="0" name="angle"/>
      <Option type="QString" value="square" name="cap_style"/>
      <Option type="QString" value="231,113,72,255" name="color"/>
      <Option type="QString" value="1" name="horizontal_anchor_point"/>
      <Option type="QString" value="bevel" name="joinstyle"/>
      <Option type="QString" value="diamond" name="name"/>
      <Option type="QString" value="0,0" name="offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="offset_unit"/>
      <Option type="QString" value="165,81,51,255" name="outline_color"/>
      <Option type="QString" value="solid" name="outline_style"/>
      <Option type="QString" value="0.2" name="outline_width"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="outline_width_map_unit_scale"/>
      <Option type="QString" value="MM" name="outline_width_unit"/>
      <Option type="QString" value="diameter" name="scale_method"/>
      <Option type="QString" value="3" name="size"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="size_map_unit_scale"/>
      <Option type="QString" value="MM" name="size_unit"/>
      <Option type="QString" value="1" name="vertical_anchor_point"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileMarkerSymbol>
 </elevation>
 <renderer-v2 symbollevels="0" type="singleSymbol" forceraster="0" enableorderby="0" referencescale="-1">
  <symbols>
   <symbol type="fill" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="0">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </data_defined_properties>
    <layer id="{d5ff5b7c-dd2b-4eb0-b5c2-d6b79b7e1d0c}" class="SimpleLine" locked="0" pass="0" enabled="1">
     <Option type="Map">
      <Option type="QString" value="0" name="align_dash_pattern"/>
      <Option type="QString" value="square" name="capstyle"/>
      <Option type="QString" value="5;2" name="customdash"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="customdash_map_unit_scale"/>
      <Option type="QString" value="MM" name="customdash_unit"/>
      <Option type="QString" value="0" name="dash_pattern_offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="dash_pattern_offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="dash_pattern_offset_unit"/>
      <Option type="QString" value="0" name="draw_inside_polygon"/>
      <Option type="QString" value="bevel" name="joinstyle"/>
      <Option type="QString" value="228,26,28,255" name="line_color"/>
      <Option type="QString" value="solid" name="line_style"/>
      <Option type="QString" value="0.3" name="line_width"/>
      <Option type="QString" value="MM" name="line_width_unit"/>
      <Option type="QString" value="0" name="offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="offset_unit"/>
      <Option type="QString" value="0" name="ring_filter"/>
      <Option type="QString" value="0" name="trim_distance_end"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_end_map_unit_scale"/>
      <Option type="QString" value="MM" name="trim_distance_end_unit"/>
      <Option type="QString" value="0" name="trim_distance_start"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_start_map_unit_scale"/>
      <Option type="QString" value="MM" name="trim_distance_start_unit"/>
      <Option type="QString" value="0" name="tweak_dash_pattern_on_corners"/>
      <Option type="QString" value="0" name="use_custom_dash"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="width_map_unit_scale"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </symbols>
  <rotation/>
  <sizescale/>
 </renderer-v2>
 <selection mode="Default">
  <selectionColor invalid="1"/>
  <selectionSymbol>
   <symbol type="fill" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </data_defined_properties>
    <layer id="{b6c2df8b-e1cb-42cc-b411-0edc221ae1f1}" class="SimpleFill" locked="0" pass="0" enabled="1">
     <Option type="Map">
      <Option type="QString" value="3x:0,0,0,0,0,0" name="border_width_map_unit_scale"/>
      <Option type="QString" value="0,0,255,255" name="color"/>
      <Option type="QString" value="bevel" name="joinstyle"/>
      <Option type="QString" value="0,0" name="offset"/>
      <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
      <Option type="QString" value="MM" name="offset_unit"/>
      <Option type="QString" value="35,35,35,255" name="outline_color"/>
      <Option type="QString" value="solid" name="outline_style"/>
      <Option type="QString" value="0.26" name="outline_width"/>
      <Option type="QString" value="MM" name="outline_width_unit"/>
      <Option type="QString" value="solid" name="style"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </selectionSymbol>
 </selection>
 <labeling type="simple">
  <settings calloutType="simple">
   <text-style textOrientation="horizontal" legendString="Aa" textColor="45,40,192,255" useSubstitutions="0" forcedBold="0" capitalization="0" fontUnderline="0" allowHtml="0" isExpression="0" fontSize="7" multilineHeightUnit="Percentage" fontLetterSpacing="0" fontSizeUnit="Point" previewBkgrdColor="255,255,255,255" fontStrikeout="0" textOpacity="1" fontItalic="0" fontSizeMapUnitScale="3x:0,0,0,0,0,0" fontFamily="MS Shell Dlg 2" fieldName="site_name" namedStyle="Bold" fontWordSpacing="0" blendMode="0" fontWeight="75" multilineHeight="1" forcedItalic="0" fontKerning="1">
    <families/>
    <text-buffer bufferSizeUnits="MM" bufferSizeMapUnitScale="3x:0,0,0,0,0,0" bufferNoFill="1" bufferJoinStyle="128" bufferSize="1" bufferColor="255,255,255,255" bufferBlendMode="0" bufferDraw="0" bufferOpacity="1"/>
    <text-mask maskOpacity="1" maskEnabled="0" maskSize="1.5" maskSizeMapUnitScale="3x:0,0,0,0,0,0" maskSizeUnits="MM" maskedSymbolLayers="" maskJoinStyle="128" maskType="0"/>
    <background shapeRotationType="0" shapeSizeUnit="MM" shapeSizeX="0" shapeJoinStyle="64" shapeRadiiMapUnitScale="3x:0,0,0,0,0,0" shapeOpacity="1" shapeRadiiY="0" shapeRadiiX="0" shapeBorderWidthUnit="MM" shapeRotation="0" shapeType="0" shapeBorderColor="128,128,128,255" shapeBorderWidthMapUnitScale="3x:0,0,0,0,0,0" shapeBlendMode="0" shapeOffsetMapUnitScale="3x:0,0,0,0,0,0" shapeSizeMapUnitScale="3x:0,0,0,0,0,0" shapeDraw="0" shapeOffsetY="0" shapeFillColor="255,255,255,255" shapeRadiiUnit="MM" shapeBorderWidth="0" shapeSizeType="0" shapeSizeY="0" shapeSVGFile="" shapeOffsetUnit="MM" shapeOffsetX="0">
     <symbol type="marker" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="markerSymbol">
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" value="" name="name"/>
        <Option name="properties"/>
        <Option type="QString" value="collection" name="type"/>
       </Option>
      </data_defined_properties>
      <layer id="" class="SimpleMarker" locked="0" pass="0" enabled="1">
       <Option type="Map">
        <Option type="QString" value="0" name="angle"/>
        <Option type="QString" value="square" name="cap_style"/>
        <Option type="QString" value="231,113,72,255" name="color"/>
        <Option type="QString" value="1" name="horizontal_anchor_point"/>
        <Option type="QString" value="bevel" name="joinstyle"/>
        <Option type="QString" value="circle" name="name"/>
        <Option type="QString" value="0,0" name="offset"/>
        <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
        <Option type="QString" value="MM" name="offset_unit"/>
        <Option type="QString" value="35,35,35,255" name="outline_color"/>
        <Option type="QString" value="solid" name="outline_style"/>
        <Option type="QString" value="0" name="outline_width"/>
        <Option type="QString" value="3x:0,0,0,0,0,0" name="outline_width_map_unit_scale"/>
        <Option type="QString" value="MM" name="outline_width_unit"/>
        <Option type="QString" value="diameter" name="scale_method"/>
        <Option type="QString" value="2" name="size"/>
        <Option type="QString" value="3x:0,0,0,0,0,0" name="size_map_unit_scale"/>
        <Option type="QString" value="MM" name="size_unit"/>
        <Option type="QString" value="1" name="vertical_anchor_point"/>
       </Option>
       <data_defined_properties>
        <Option type="Map">
         <Option type="QString" value="" name="name"/>
         <Option name="properties"/>
         <Option type="QString" value="collection" name="type"/>
        </Option>
       </data_defined_properties>
      </layer>
     </symbol>
     <symbol type="fill" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="fillSymbol">
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" value="" name="name"/>
        <Option name="properties"/>
        <Option type="QString" value="collection" name="type"/>
       </Option>
      </data_defined_properties>
      <layer id="" class="SimpleFill" locked="0" pass="0" enabled="1">
       <Option type="Map">
        <Option type="QString" value="3x:0,0,0,0,0,0" name="border_width_map_unit_scale"/>
        <Option type="QString" value="255,255,255,255" name="color"/>
        <Option type="QString" value="bevel" name="joinstyle"/>
        <Option type="QString" value="0,0" name="offset"/>
        <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
        <Option type="QString" value="MM" name="offset_unit"/>
        <Option type="QString" value="128,128,128,255" name="outline_color"/>
        <Option type="QString" value="no" name="outline_style"/>
        <Option type="QString" value="0" name="outline_width"/>
        <Option type="QString" value="MM" name="outline_width_unit"/>
        <Option type="QString" value="solid" name="style"/>
       </Option>
       <data_defined_properties>
        <Option type="Map">
         <Option type="QString" value="" name="name"/>
         <Option name="properties"/>
         <Option type="QString" value="collection" name="type"/>
        </Option>
       </data_defined_properties>
      </layer>
     </symbol>
    </background>
    <shadow shadowOffsetAngle="135" shadowOffsetDist="1" shadowScale="100" shadowOffsetUnit="MM" shadowUnder="0" shadowOffsetMapUnitScale="3x:0,0,0,0,0,0" shadowRadius="1.5" shadowDraw="0" shadowRadiusMapUnitScale="3x:0,0,0,0,0,0" shadowRadiusUnit="MM" shadowRadiusAlphaOnly="0" shadowColor="0,0,0,255" shadowOpacity="0.69999999999999996" shadowOffsetGlobal="1" shadowBlendMode="6"/>
    <dd_properties>
     <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
    </dd_properties>
    <substitutions/>
   </text-style>
   <text-format autoWrapLength="1" reverseDirectionSymbol="0" formatNumbers="0" addDirectionSymbol="0" plussign="0" multilineAlign="3" useMaxLineLengthForAutoWrap="1" leftDirectionSymbol="&lt;" wrapChar="" rightDirectionSymbol=">" decimals="3" placeDirectionSymbol="0"/>
   <placement placementFlags="10" distUnits="MM" geometryGenerator="" offsetUnits="MM" lineAnchorType="0" lineAnchorClipping="0" xOffset="0" placement="0" maxCurvedCharAngleIn="25" repeatDistance="0" fitInPolygonOnly="0" overrunDistanceMapUnitScale="3x:0,0,0,0,0,0" rotationAngle="0" offsetType="0" centroidWhole="0" allowDegraded="0" geometryGeneratorEnabled="0" dist="0" distMapUnitScale="3x:0,0,0,0,0,0" lineAnchorTextPoint="CenterOfText" maxCurvedCharAngleOut="-25" labelOffsetMapUnitScale="3x:0,0,0,0,0,0" layerType="PolygonGeometry" polygonPlacementFlags="2" repeatDistanceMapUnitScale="3x:0,0,0,0,0,0" quadOffset="4" priority="5" yOffset="0" rotationUnit="AngleDegrees" overrunDistance="0" overrunDistanceUnit="MM" centroidInside="0" predefinedPositionOrder="TR,TL,BR,BL,R,L,TSR,BSR" lineAnchorPercent="0.5" geometryGeneratorType="PointGeometry" repeatDistanceUnits="MM" overlapHandling="PreventOverlap" preserveRotation="1"/>
   <rendering obstacleType="1" fontMinPixelSize="3" labelPerPart="0" limitNumLabels="0" minFeatureSize="0" upsidedownLabels="0" maxNumLabels="2000" fontMaxPixelSize="10000" unplacedVisibility="0" obstacle="1" mergeLines="0" fontLimitPixelSize="0" scaleMin="0" scaleVisibility="0" drawLabels="1" scaleMax="0" zIndex="0" obstacleFactor="1"/>
   <dd_properties>
    <Option type="Map">
     <Option type="QString" value="" name="name"/>
     <Option name="properties"/>
     <Option type="QString" value="collection" name="type"/>
    </Option>
   </dd_properties>
   <callout type="simple">
    <Option type="Map">
     <Option type="QString" value="pole_of_inaccessibility" name="anchorPoint"/>
     <Option type="int" value="0" name="blendMode"/>
     <Option type="Map" name="ddProperties">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
     </Option>
     <Option type="bool" value="false" name="drawToAllParts"/>
     <Option type="QString" value="1" name="enabled"/>
     <Option type="QString" value="point_on_exterior" name="labelAnchorPoint"/>
     <Option type="QString" value="&lt;symbol type=&quot;line&quot; force_rhr=&quot;0&quot; frame_rate=&quot;10&quot; alpha=&quot;1&quot; clip_to_extent=&quot;1&quot; is_animated=&quot;0&quot; name=&quot;symbol&quot;>&lt;data_defined_properties>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; value=&quot;&quot; name=&quot;name&quot;/>&lt;Option name=&quot;properties&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;collection&quot; name=&quot;type&quot;/>&lt;/Option>&lt;/data_defined_properties>&lt;layer id=&quot;{3d5798d3-0c9f-40b8-8f3d-4570e848daa4}&quot; class=&quot;SimpleLine&quot; locked=&quot;0&quot; pass=&quot;0&quot; enabled=&quot;1&quot;>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;align_dash_pattern&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;square&quot; name=&quot;capstyle&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;5;2&quot; name=&quot;customdash&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;customdash_map_unit_scale&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;customdash_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;dash_pattern_offset&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;dash_pattern_offset_map_unit_scale&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;dash_pattern_offset_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;draw_inside_polygon&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;bevel&quot; name=&quot;joinstyle&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;60,60,60,255&quot; name=&quot;line_color&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;solid&quot; name=&quot;line_style&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0.3&quot; name=&quot;line_width&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;line_width_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;offset&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;offset_map_unit_scale&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;offset_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;ring_filter&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;trim_distance_end&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;trim_distance_end_map_unit_scale&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;trim_distance_end_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;trim_distance_start&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;trim_distance_start_map_unit_scale&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;MM&quot; name=&quot;trim_distance_start_unit&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;tweak_dash_pattern_on_corners&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;0&quot; name=&quot;use_custom_dash&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;3x:0,0,0,0,0,0&quot; name=&quot;width_map_unit_scale&quot;/>&lt;/Option>&lt;data_defined_properties>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; value=&quot;&quot; name=&quot;name&quot;/>&lt;Option name=&quot;properties&quot;/>&lt;Option type=&quot;QString&quot; value=&quot;collection&quot; name=&quot;type&quot;/>&lt;/Option>&lt;/data_defined_properties>&lt;/layer>&lt;/symbol>" name="lineSymbol"/>
     <Option type="double" value="0" name="minLength"/>
     <Option type="QString" value="3x:0,0,0,0,0,0" name="minLengthMapUnitScale"/>
     <Option type="QString" value="MM" name="minLengthUnit"/>
     <Option type="double" value="0.4" name="offsetFromAnchor"/>
     <Option type="QString" value="3x:0,0,0,0,0,0" name="offsetFromAnchorMapUnitScale"/>
     <Option type="QString" value="MM" name="offsetFromAnchorUnit"/>
     <Option type="double" value="0" name="offsetFromLabel"/>
     <Option type="QString" value="3x:0,0,0,0,0,0" name="offsetFromLabelMapUnitScale"/>
     <Option type="QString" value="MM" name="offsetFromLabelUnit"/>
    </Option>
   </callout>
  </settings>
 </labeling>
 <customproperties>
  <Option type="Map">
   <Option type="QString" value="offline" name="QFieldSync/action"/>
   <Option type="QString" value="{}" name="QFieldSync/attachment_naming"/>
   <Option type="QString" value="offline" name="QFieldSync/cloud_action"/>
   <Option type="QString" value="" name="QFieldSync/geometry_locked_expression"/>
   <Option type="QString" value="{}" name="QFieldSync/photo_naming"/>
   <Option type="QString" value="{&quot;parcels_c51f3997_8de2_4c43_b598_99f9e6534a61_site_id_sites_786dfbe7_58ad_4a7e_8c19_40bf41880138_site_id&quot;: 4}" name="QFieldSync/relationship_maximum_visible"/>
   <Option type="int" value="30" name="QFieldSync/tracking_distance_requirement_minimum_meters"/>
   <Option type="int" value="1" name="QFieldSync/tracking_erroneous_distance_safeguard_maximum_meters"/>
   <Option type="int" value="0" name="QFieldSync/tracking_measurement_type"/>
   <Option type="int" value="30" name="QFieldSync/tracking_time_requirement_interval_seconds"/>
   <Option type="int" value="0" name="QFieldSync/value_map_button_interface_threshold"/>
   <Option type="int" value="0" name="embeddedWidgets/count"/>
   <Option name="variableNames"/>
   <Option name="variableValues"/>
  </Option>
 </customproperties>
 <blendMode>0</blendMode>
 <featureBlendMode>0</featureBlendMode>
 <layerOpacity>1</layerOpacity>
 <SingleCategoryDiagramRenderer diagramType="Histogram" attributeLegend="1">
  <DiagramCategory maxScaleDenominator="1e+08" lineSizeScale="3x:0,0,0,0,0,0" sizeType="MM" width="15" spacing="5" backgroundColor="#ffffff" showAxis="1" barWidth="5" labelPlacementMethod="XHeight" backgroundAlpha="255" diagramOrientation="Up" scaleBasedVisibility="0" rotationOffset="270" enabled="0" penColor="#000000" lineSizeType="MM" minScaleDenominator="0" scaleDependency="Area" spacingUnitScale="3x:0,0,0,0,0,0" penWidth="0" height="15" spacingUnit="MM" sizeScale="3x:0,0,0,0,0,0" direction="0" minimumSize="0" opacity="1" penAlpha="255">
   <fontProperties strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" style="" underline="0"/>
   <attribute field="" colorOpacity="1" label="" color="#000000"/>
   <axisSymbol>
    <symbol type="line" force_rhr="0" frame_rate="10" alpha="1" clip_to_extent="1" is_animated="0" name="">
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" value="" name="name"/>
       <Option name="properties"/>
       <Option type="QString" value="collection" name="type"/>
      </Option>
     </data_defined_properties>
     <layer id="{379d797b-f1df-4296-876b-dc97c407690c}" class="SimpleLine" locked="0" pass="0" enabled="1">
      <Option type="Map">
       <Option type="QString" value="0" name="align_dash_pattern"/>
       <Option type="QString" value="square" name="capstyle"/>
       <Option type="QString" value="5;2" name="customdash"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="customdash_map_unit_scale"/>
       <Option type="QString" value="MM" name="customdash_unit"/>
       <Option type="QString" value="0" name="dash_pattern_offset"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="dash_pattern_offset_map_unit_scale"/>
       <Option type="QString" value="MM" name="dash_pattern_offset_unit"/>
       <Option type="QString" value="0" name="draw_inside_polygon"/>
       <Option type="QString" value="bevel" name="joinstyle"/>
       <Option type="QString" value="35,35,35,255" name="line_color"/>
       <Option type="QString" value="solid" name="line_style"/>
       <Option type="QString" value="0.26" name="line_width"/>
       <Option type="QString" value="MM" name="line_width_unit"/>
       <Option type="QString" value="0" name="offset"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="offset_map_unit_scale"/>
       <Option type="QString" value="MM" name="offset_unit"/>
       <Option type="QString" value="0" name="ring_filter"/>
       <Option type="QString" value="0" name="trim_distance_end"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_end_map_unit_scale"/>
       <Option type="QString" value="MM" name="trim_distance_end_unit"/>
       <Option type="QString" value="0" name="trim_distance_start"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="trim_distance_start_map_unit_scale"/>
       <Option type="QString" value="MM" name="trim_distance_start_unit"/>
       <Option type="QString" value="0" name="tweak_dash_pattern_on_corners"/>
       <Option type="QString" value="0" name="use_custom_dash"/>
       <Option type="QString" value="3x:0,0,0,0,0,0" name="width_map_unit_scale"/>
      </Option>
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" value="" name="name"/>
        <Option name="properties"/>
        <Option type="QString" value="collection" name="type"/>
       </Option>
      </data_defined_properties>
     </layer>
    </symbol>
   </axisSymbol>
  </DiagramCategory>
 </SingleCategoryDiagramRenderer>
 <DiagramLayerSettings priority="0" dist="0" zIndex="0" obstacle="0" linePlacementFlags="18" placement="1" showAll="1">
  <properties>
   <Option type="Map">
    <Option type="QString" value="" name="name"/>
    <Option name="properties"/>
    <Option type="QString" value="collection" name="type"/>
   </Option>
  </properties>
 </DiagramLayerSettings>
 <geometryOptions removeDuplicateNodes="0" geometryPrecision="0">
  <activeChecks/>
  <checkConfiguration type="Map">
   <Option type="Map" name="QgsGeometryGapCheck">
    <Option type="double" value="0" name="allowedGapsBuffer"/>
    <Option type="bool" value="false" name="allowedGapsEnabled"/>
    <Option type="QString" value="" name="allowedGapsLayer"/>
   </Option>
  </checkConfiguration>
 </geometryOptions>
 <legend type="default-vector" showLabelLegend="0"/>
 <referencedLayers/>
 <fieldConfiguration>
  <field configurationFlags="NoFlag" name="site_id">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" value="false" name="IsMultiline"/>
      <Option type="bool" value="false" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field configurationFlags="NoFlag" name="site_name">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" value="false" name="IsMultiline"/>
      <Option type="bool" value="false" name="UseHtml"/>
     </Option>
    </config>
   </editWidget>
  </field>
 </fieldConfiguration>
 <aliases>
  <alias field="site_id" name="" index="0"/>
  <alias field="site_name" name="" index="1"/>
 </aliases>
 <splitPolicies>
  <policy field="site_id" policy="Duplicate"/>
  <policy field="site_name" policy="Duplicate"/>
 </splitPolicies>
 <defaults>
  <default field="site_id" applyOnUpdate="0" expression=""/>
  <default field="site_name" applyOnUpdate="0" expression=""/>
 </defaults>
 <constraints>
  <constraint field="site_id" constraints="3" notnull_strength="1" exp_strength="0" unique_strength="1"/>
  <constraint field="site_name" constraints="3" notnull_strength="1" exp_strength="0" unique_strength="1"/>
 </constraints>
 <constraintExpressions>
  <constraint field="site_id" exp="" desc=""/>
  <constraint field="site_name" exp="" desc=""/>
 </constraintExpressions>
 <expressionfields/>
 <attributeactions>
  <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
 </attributeactions>
 <attributetableconfig sortOrder="0" actionWidgetStyle="dropDown" sortExpression="">
  <columns>
   <column type="field" width="-1" hidden="0" name="site_id"/>
   <column type="field" width="-1" hidden="0" name="site_name"/>
   <column type="actions" width="-1" hidden="1"/>
  </columns>
 </attributetableconfig>
 <conditionalstyles>
  <rowstyles/>
  <fieldstyles/>
 </conditionalstyles>
 <storedexpressions/>
 <editform tolerant="1"></editform>
 <editforminit/>
 <editforminitcodesource>0</editforminitcodesource>
 <editforminitfilepath></editforminitfilepath>
 <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
    geom = feature.geometry()
    control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
 <featformsuppress>0</featformsuppress>
 <editorlayout>tablayout</editorlayout>
 <attributeEditorForm>
  <labelStyle overrideLabelFont="0" overrideLabelColor="0" labelColor="0,0,0,255">
   <labelFont strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" style="" underline="0"/>
  </labelStyle>
  <attributeEditorField verticalStretch="0" horizontalStretch="0" showLabel="1" name="site_id" index="0">
   <labelStyle overrideLabelFont="0" overrideLabelColor="0" labelColor="0,0,0,255">
    <labelFont strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" style="" underline="0"/>
   </labelStyle>
  </attributeEditorField>
  <attributeEditorField verticalStretch="0" horizontalStretch="0" showLabel="1" name="site_name" index="1">
   <labelStyle overrideLabelFont="0" overrideLabelColor="0" labelColor="0,0,0,255">
    <labelFont strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" style="" underline="0"/>
   </labelStyle>
  </attributeEditorField>
  <attributeEditorRelation verticalStretch="0" forceSuppressFormPopup="0" horizontalStretch="0" nmRelationId="" label="Components" relationWidgetTypeId="relation_editor" showLabel="1" relation="" name="">
   <labelStyle overrideLabelFont="0" overrideLabelColor="0" labelColor="0,0,0,255">
    <labelFont strikethrough="0" bold="0" italic="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" style="" underline="0"/>
   </labelStyle>
   <editor_configuration type="Map">
    <Option type="bool" value="false" name="allow_add_child_feature_with_no_geometry"/>
    <Option type="QString" value="AllButtons" name="buttons"/>
    <Option type="bool" value="true" name="show_first_feature"/>
   </editor_configuration>
  </attributeEditorRelation>
 </attributeEditorForm>
 <editable>
  <field editable="1" name="site_id"/>
  <field editable="1" name="site_name"/>
 </editable>
 <labelOnTop>
  <field labelOnTop="0" name="site_id"/>
  <field labelOnTop="0" name="site_name"/>
 </labelOnTop>
 <reuseLastValue>
  <field reuseLastValue="0" name="site_id"/>
  <field reuseLastValue="0" name="site_name"/>
 </reuseLastValue>
 <dataDefinedFieldProperties/>
 <widgets>
  <widget name="parcels_c51f3997_8de2_4c43_b598_99f9e6534a61_site_id_sites_786dfbe7_58ad_4a7e_8c19_40bf41880138_site_id">
   <config type="Map">
    <Option type="bool" value="false" name="force-suppress-popup"/>
    <Option type="QString" value="" name="nm-rel"/>
   </config>
  </widget>
 </widgets>
 <previewExpression>"site_name"</previewExpression>
 <mapTip enabled="1"></mapTip>
 <layerGeometryType>2</layerGeometryType>
</qgis>
', '<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:se="http://www.opengis.net/se" version="1.1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
 <NamedLayer>
  <se:Name>sites</se:Name>
  <UserStyle>
   <se:Name>sites</se:Name>
   <se:FeatureTypeStyle>
    <se:Rule>
     <se:Name>Single symbol</se:Name>
     <se:LineSymbolizer>
      <se:Stroke>
       <se:SvgParameter name="stroke">#e41a1c</se:SvgParameter>
       <se:SvgParameter name="stroke-width">1</se:SvgParameter>
       <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
       <se:SvgParameter name="stroke-linecap">square</se:SvgParameter>
      </se:Stroke>
     </se:LineSymbolizer>
    </se:Rule>
    <se:Rule>
     <se:TextSymbolizer>
      <se:Label>
       <ogc:PropertyName>site_name</ogc:PropertyName>
      </se:Label>
      <se:Font>
       <se:SvgParameter name="font-family">MS Shell Dlg 2</se:SvgParameter>
       <se:SvgParameter name="font-size">9</se:SvgParameter>
       <se:SvgParameter name="font-weight">bold</se:SvgParameter>
      </se:Font>
      <se:LabelPlacement>
       <se:PointPlacement>
        <se:AnchorPoint>
         <se:AnchorPointX>0</se:AnchorPointX>
         <se:AnchorPointY>0.5</se:AnchorPointY>
        </se:AnchorPoint>
       </se:PointPlacement>
      </se:LabelPlacement>
      <se:Fill>
       <se:SvgParameter name="fill">#2d28c0</se:SvgParameter>
      </se:Fill>
      <se:VendorOption name="maxDisplacement">1</se:VendorOption>
     </se:TextSymbolizer>
    </se:Rule>
   </se:FeatureTypeStyle>
  </UserStyle>
 </NamedLayer>
</StyledLayerDescriptor>
', true, 'Thu Sep 26 18:15:52 2024', 'dominic.hinchley', NULL, '2024-09-06 12:16:51.361735', 'Polygon');
INSERT INTO qgis.layer_styles VALUES (14, 'land_terrier', 'metadata', 'qgis_layer_metadata', 'extent', 'qgis_layer_metadata', '<!DOCTYPE qgis PUBLIC ''http://mrcc.com/qgis.dtd'' ''SYSTEM''>
<qgis simplifyDrawingHints="1" readOnly="0" version="3.34.8-Prizren" minScale="100000000" simplifyDrawingTol="1" simplifyLocal="1" simplifyMaxScale="1" hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories" maxScale="0" simplifyAlgorithm="0" symbologyReferenceScale="-1" labelsEnabled="0">
 <flags>
  <Identifiable>1</Identifiable>
  <Removable>1</Removable>
  <Searchable>1</Searchable>
  <Private>0</Private>
 </flags>
 <temporal limitMode="0" accumulate="0" fixedDuration="0" mode="0" endExpression="" durationUnit="min" durationField="id" endField="" startExpression="" enabled="0" startField="update_time">
  <fixedRange>
   <start></start>
   <end></end>
  </fixedRange>
 </temporal>
 <elevation zscale="1" binding="Centroid" zoffset="0" type="IndividualFeatures" symbology="Line" showMarkerSymbolInSurfacePlots="0" extrusionEnabled="0" respectLayerSymbol="1" extrusion="0" clamping="Terrain">
  <data-defined-properties>
   <Option type="Map">
    <Option name="name" value="" type="QString"/>
    <Option name="properties"/>
    <Option name="type" value="collection" type="QString"/>
   </Option>
  </data-defined-properties>
  <profileLineSymbol>
   <symbol name="" is_animated="0" type="line" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
    <data_defined_properties>
     <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
     </Option>
    </data_defined_properties>
    <layer class="SimpleLine" pass="0" locked="0" id="{fc254a07-15ff-4d11-aec9-24faa94fa5ae}" enabled="1">
     <Option type="Map">
      <Option name="align_dash_pattern" value="0" type="QString"/>
      <Option name="capstyle" value="square" type="QString"/>
      <Option name="customdash" value="5;2" type="QString"/>
      <Option name="customdash_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="customdash_unit" value="MM" type="QString"/>
      <Option name="dash_pattern_offset" value="0" type="QString"/>
      <Option name="dash_pattern_offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="dash_pattern_offset_unit" value="MM" type="QString"/>
      <Option name="draw_inside_polygon" value="0" type="QString"/>
      <Option name="joinstyle" value="bevel" type="QString"/>
      <Option name="line_color" value="141,90,153,255" type="QString"/>
      <Option name="line_style" value="solid" type="QString"/>
      <Option name="line_width" value="0.6" type="QString"/>
      <Option name="line_width_unit" value="MM" type="QString"/>
      <Option name="offset" value="0" type="QString"/>
      <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="offset_unit" value="MM" type="QString"/>
      <Option name="ring_filter" value="0" type="QString"/>
      <Option name="trim_distance_end" value="0" type="QString"/>
      <Option name="trim_distance_end_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="trim_distance_end_unit" value="MM" type="QString"/>
      <Option name="trim_distance_start" value="0" type="QString"/>
      <Option name="trim_distance_start_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="trim_distance_start_unit" value="MM" type="QString"/>
      <Option name="tweak_dash_pattern_on_corners" value="0" type="QString"/>
      <Option name="use_custom_dash" value="0" type="QString"/>
      <Option name="width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileLineSymbol>
  <profileFillSymbol>
   <symbol name="" is_animated="0" type="fill" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
    <data_defined_properties>
     <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
     </Option>
    </data_defined_properties>
    <layer class="SimpleFill" pass="0" locked="0" id="{20ef6b4d-1262-410c-9e3e-16b74ee92de1}" enabled="1">
     <Option type="Map">
      <Option name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="color" value="141,90,153,255" type="QString"/>
      <Option name="joinstyle" value="bevel" type="QString"/>
      <Option name="offset" value="0,0" type="QString"/>
      <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="offset_unit" value="MM" type="QString"/>
      <Option name="outline_color" value="101,64,109,255" type="QString"/>
      <Option name="outline_style" value="solid" type="QString"/>
      <Option name="outline_width" value="0.2" type="QString"/>
      <Option name="outline_width_unit" value="MM" type="QString"/>
      <Option name="style" value="solid" type="QString"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileFillSymbol>
  <profileMarkerSymbol>
   <symbol name="" is_animated="0" type="marker" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
    <data_defined_properties>
     <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
     </Option>
    </data_defined_properties>
    <layer class="SimpleMarker" pass="0" locked="0" id="{ea9426ef-d80e-4d69-85e2-d13f7c4f7981}" enabled="1">
     <Option type="Map">
      <Option name="angle" value="0" type="QString"/>
      <Option name="cap_style" value="square" type="QString"/>
      <Option name="color" value="141,90,153,255" type="QString"/>
      <Option name="horizontal_anchor_point" value="1" type="QString"/>
      <Option name="joinstyle" value="bevel" type="QString"/>
      <Option name="name" value="diamond" type="QString"/>
      <Option name="offset" value="0,0" type="QString"/>
      <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="offset_unit" value="MM" type="QString"/>
      <Option name="outline_color" value="101,64,109,255" type="QString"/>
      <Option name="outline_style" value="solid" type="QString"/>
      <Option name="outline_width" value="0.2" type="QString"/>
      <Option name="outline_width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="outline_width_unit" value="MM" type="QString"/>
      <Option name="scale_method" value="diameter" type="QString"/>
      <Option name="size" value="3" type="QString"/>
      <Option name="size_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="size_unit" value="MM" type="QString"/>
      <Option name="vertical_anchor_point" value="1" type="QString"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileMarkerSymbol>
 </elevation>
 <renderer-v2 forceraster="0" type="singleSymbol" symbollevels="0" enableorderby="0" referencescale="-1">
  <symbols>
   <symbol name="0" is_animated="0" type="fill" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
    <data_defined_properties>
     <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
     </Option>
    </data_defined_properties>
    <layer class="SimpleFill" pass="0" locked="0" id="{5c644b48-1c0c-4bb8-96e2-acd2f0b911fc}" enabled="1">
     <Option type="Map">
      <Option name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="color" value="177,29,170,0" type="QString"/>
      <Option name="joinstyle" value="bevel" type="QString"/>
      <Option name="offset" value="0,0" type="QString"/>
      <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="offset_unit" value="MM" type="QString"/>
      <Option name="outline_color" value="177,29,170,255" type="QString"/>
      <Option name="outline_style" value="dash" type="QString"/>
      <Option name="outline_width" value="0.4" type="QString"/>
      <Option name="outline_width_unit" value="MM" type="QString"/>
      <Option name="style" value="solid" type="QString"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </symbols>
  <rotation/>
  <sizescale/>
 </renderer-v2>
 <selection mode="Default">
  <selectionColor invalid="1"/>
  <selectionSymbol>
   <symbol name="" is_animated="0" type="fill" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
    <data_defined_properties>
     <Option type="Map">
      <Option name="name" value="" type="QString"/>
      <Option name="properties"/>
      <Option name="type" value="collection" type="QString"/>
     </Option>
    </data_defined_properties>
    <layer class="SimpleFill" pass="0" locked="0" id="{7d5f256a-dcf8-4680-84e9-11a231290b7b}" enabled="1">
     <Option type="Map">
      <Option name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="color" value="0,0,255,255" type="QString"/>
      <Option name="joinstyle" value="bevel" type="QString"/>
      <Option name="offset" value="0,0" type="QString"/>
      <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      <Option name="offset_unit" value="MM" type="QString"/>
      <Option name="outline_color" value="35,35,35,255" type="QString"/>
      <Option name="outline_style" value="solid" type="QString"/>
      <Option name="outline_width" value="0.26" type="QString"/>
      <Option name="outline_width_unit" value="MM" type="QString"/>
      <Option name="style" value="solid" type="QString"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </selectionSymbol>
 </selection>
 <customproperties>
  <Option type="Map">
   <Option name="QFieldSync/action" value="offline" type="QString"/>
   <Option name="QFieldSync/attachment_naming" value="{}" type="QString"/>
   <Option name="QFieldSync/cloud_action" value="offline" type="QString"/>
   <Option name="QFieldSync/geometry_locked_expression" value="" type="QString"/>
   <Option name="QFieldSync/photo_naming" value="{}" type="QString"/>
   <Option name="QFieldSync/relationship_maximum_visible" value="{}" type="QString"/>
   <Option name="QFieldSync/tracking_distance_requirement_minimum_meters" value="30" type="int"/>
   <Option name="QFieldSync/tracking_erroneous_distance_safeguard_maximum_meters" value="1" type="int"/>
   <Option name="QFieldSync/tracking_measurement_type" value="0" type="int"/>
   <Option name="QFieldSync/tracking_time_requirement_interval_seconds" value="30" type="int"/>
   <Option name="QFieldSync/value_map_button_interface_threshold" value="0" type="int"/>
   <Option name="embeddedWidgets/count" value="0" type="int"/>
   <Option name="variableNames"/>
   <Option name="variableValues"/>
  </Option>
 </customproperties>
 <blendMode>0</blendMode>
 <featureBlendMode>0</featureBlendMode>
 <layerOpacity>1</layerOpacity>
 <SingleCategoryDiagramRenderer diagramType="Histogram" attributeLegend="1">
  <DiagramCategory spacingUnitScale="3x:0,0,0,0,0,0" rotationOffset="270" minScaleDenominator="0" diagramOrientation="Up" lineSizeScale="3x:0,0,0,0,0,0" scaleDependency="Area" spacingUnit="MM" maxScaleDenominator="1e+08" sizeScale="3x:0,0,0,0,0,0" penWidth="0" scaleBasedVisibility="0" minimumSize="0" opacity="1" enabled="0" lineSizeType="MM" penAlpha="255" backgroundAlpha="255" showAxis="1" labelPlacementMethod="XHeight" width="15" backgroundColor="#ffffff" barWidth="5" height="15" spacing="5" penColor="#000000" sizeType="MM" direction="0">
   <fontProperties style="" underline="0" bold="0" description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" italic="0" strikethrough="0"/>
   <axisSymbol>
    <symbol name="" is_animated="0" type="line" alpha="1" clip_to_extent="1" force_rhr="0" frame_rate="10">
     <data_defined_properties>
      <Option type="Map">
       <Option name="name" value="" type="QString"/>
       <Option name="properties"/>
       <Option name="type" value="collection" type="QString"/>
      </Option>
     </data_defined_properties>
     <layer class="SimpleLine" pass="0" locked="0" id="{4c43e0d5-206d-4748-83ae-849017c5af14}" enabled="1">
      <Option type="Map">
       <Option name="align_dash_pattern" value="0" type="QString"/>
       <Option name="capstyle" value="square" type="QString"/>
       <Option name="customdash" value="5;2" type="QString"/>
       <Option name="customdash_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
       <Option name="customdash_unit" value="MM" type="QString"/>
       <Option name="dash_pattern_offset" value="0" type="QString"/>
       <Option name="dash_pattern_offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
       <Option name="dash_pattern_offset_unit" value="MM" type="QString"/>
       <Option name="draw_inside_polygon" value="0" type="QString"/>
       <Option name="joinstyle" value="bevel" type="QString"/>
       <Option name="line_color" value="35,35,35,255" type="QString"/>
       <Option name="line_style" value="solid" type="QString"/>
       <Option name="line_width" value="0.26" type="QString"/>
       <Option name="line_width_unit" value="MM" type="QString"/>
       <Option name="offset" value="0" type="QString"/>
       <Option name="offset_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
       <Option name="offset_unit" value="MM" type="QString"/>
       <Option name="ring_filter" value="0" type="QString"/>
       <Option name="trim_distance_end" value="0" type="QString"/>
       <Option name="trim_distance_end_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
       <Option name="trim_distance_end_unit" value="MM" type="QString"/>
       <Option name="trim_distance_start" value="0" type="QString"/>
       <Option name="trim_distance_start_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
       <Option name="trim_distance_start_unit" value="MM" type="QString"/>
       <Option name="tweak_dash_pattern_on_corners" value="0" type="QString"/>
       <Option name="use_custom_dash" value="0" type="QString"/>
       <Option name="width_map_unit_scale" value="3x:0,0,0,0,0,0" type="QString"/>
      </Option>
      <data_defined_properties>
       <Option type="Map">
        <Option name="name" value="" type="QString"/>
        <Option name="properties"/>
        <Option name="type" value="collection" type="QString"/>
       </Option>
      </data_defined_properties>
     </layer>
    </symbol>
   </axisSymbol>
  </DiagramCategory>
 </SingleCategoryDiagramRenderer>
 <DiagramLayerSettings linePlacementFlags="18" zIndex="0" obstacle="0" placement="1" priority="0" showAll="1" dist="0">
  <properties>
   <Option type="Map">
    <Option name="name" value="" type="QString"/>
    <Option name="properties"/>
    <Option name="type" value="collection" type="QString"/>
   </Option>
  </properties>
 </DiagramLayerSettings>
 <geometryOptions geometryPrecision="0" removeDuplicateNodes="0">
  <activeChecks/>
  <checkConfiguration type="Map">
   <Option name="QgsGeometryGapCheck" type="Map">
    <Option name="allowedGapsBuffer" value="0" type="double"/>
    <Option name="allowedGapsEnabled" value="false" type="bool"/>
    <Option name="allowedGapsLayer" value="" type="QString"/>
   </Option>
  </checkConfiguration>
 </geometryOptions>
 <legend type="default-vector" showLabelLegend="0"/>
 <referencedLayers/>
 <fieldConfiguration>
  <field name="id" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="f_table_catalog" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="f_table_schema" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="f_table_name" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="f_geometry_column" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="identifier" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="title" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="abstract" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="geometry_type" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="crs" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="layer_type" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="qmd" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="owner" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
  <field name="update_time" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option/>
    </config>
   </editWidget>
  </field>
 </fieldConfiguration>
 <aliases>
  <alias name="" index="0" field="id"/>
  <alias name="" index="1" field="f_table_catalog"/>
  <alias name="" index="2" field="f_table_schema"/>
  <alias name="" index="3" field="f_table_name"/>
  <alias name="" index="4" field="f_geometry_column"/>
  <alias name="" index="5" field="identifier"/>
  <alias name="" index="6" field="title"/>
  <alias name="" index="7" field="abstract"/>
  <alias name="" index="8" field="geometry_type"/>
  <alias name="" index="9" field="crs"/>
  <alias name="" index="10" field="layer_type"/>
  <alias name="" index="11" field="qmd"/>
  <alias name="" index="12" field="owner"/>
  <alias name="" index="13" field="update_time"/>
 </aliases>
 <splitPolicies>
  <policy policy="Duplicate" field="id"/>
  <policy policy="Duplicate" field="f_table_catalog"/>
  <policy policy="Duplicate" field="f_table_schema"/>
  <policy policy="Duplicate" field="f_table_name"/>
  <policy policy="Duplicate" field="f_geometry_column"/>
  <policy policy="Duplicate" field="identifier"/>
  <policy policy="Duplicate" field="title"/>
  <policy policy="Duplicate" field="abstract"/>
  <policy policy="Duplicate" field="geometry_type"/>
  <policy policy="Duplicate" field="crs"/>
  <policy policy="Duplicate" field="layer_type"/>
  <policy policy="Duplicate" field="qmd"/>
  <policy policy="Duplicate" field="owner"/>
  <policy policy="Duplicate" field="update_time"/>
 </splitPolicies>
 <defaults>
  <default expression="" field="id" applyOnUpdate="0"/>
  <default expression="" field="f_table_catalog" applyOnUpdate="0"/>
  <default expression="" field="f_table_schema" applyOnUpdate="0"/>
  <default expression="" field="f_table_name" applyOnUpdate="0"/>
  <default expression="" field="f_geometry_column" applyOnUpdate="0"/>
  <default expression="" field="identifier" applyOnUpdate="0"/>
  <default expression="" field="title" applyOnUpdate="0"/>
  <default expression="" field="abstract" applyOnUpdate="0"/>
  <default expression="" field="geometry_type" applyOnUpdate="0"/>
  <default expression="" field="crs" applyOnUpdate="0"/>
  <default expression="" field="layer_type" applyOnUpdate="0"/>
  <default expression="" field="qmd" applyOnUpdate="0"/>
  <default expression="" field="owner" applyOnUpdate="0"/>
  <default expression="" field="update_time" applyOnUpdate="0"/>
 </defaults>
 <constraints>
  <constraint exp_strength="0" field="id" constraints="3" notnull_strength="1" unique_strength="1"/>
  <constraint exp_strength="0" field="f_table_catalog" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="f_table_schema" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="f_table_name" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="f_geometry_column" constraints="0" notnull_strength="0" unique_strength="0"/>
  <constraint exp_strength="0" field="identifier" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="title" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="abstract" constraints="0" notnull_strength="0" unique_strength="0"/>
  <constraint exp_strength="0" field="geometry_type" constraints="0" notnull_strength="0" unique_strength="0"/>
  <constraint exp_strength="0" field="crs" constraints="0" notnull_strength="0" unique_strength="0"/>
  <constraint exp_strength="0" field="layer_type" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="qmd" constraints="1" notnull_strength="1" unique_strength="0"/>
  <constraint exp_strength="0" field="owner" constraints="0" notnull_strength="0" unique_strength="0"/>
  <constraint exp_strength="0" field="update_time" constraints="0" notnull_strength="0" unique_strength="0"/>
 </constraints>
 <constraintExpressions>
  <constraint exp="" desc="" field="id"/>
  <constraint exp="" desc="" field="f_table_catalog"/>
  <constraint exp="" desc="" field="f_table_schema"/>
  <constraint exp="" desc="" field="f_table_name"/>
  <constraint exp="" desc="" field="f_geometry_column"/>
  <constraint exp="" desc="" field="identifier"/>
  <constraint exp="" desc="" field="title"/>
  <constraint exp="" desc="" field="abstract"/>
  <constraint exp="" desc="" field="geometry_type"/>
  <constraint exp="" desc="" field="crs"/>
  <constraint exp="" desc="" field="layer_type"/>
  <constraint exp="" desc="" field="qmd"/>
  <constraint exp="" desc="" field="owner"/>
  <constraint exp="" desc="" field="update_time"/>
 </constraintExpressions>
 <expressionfields/>
 <attributeactions>
  <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
 </attributeactions>
 <attributetableconfig actionWidgetStyle="dropDown" sortExpression="" sortOrder="0">
  <columns>
   <column width="-1" hidden="0" name="id" type="field"/>
   <column width="-1" hidden="0" name="f_table_catalog" type="field"/>
   <column width="-1" hidden="0" name="f_table_schema" type="field"/>
   <column width="-1" hidden="0" name="f_table_name" type="field"/>
   <column width="-1" hidden="0" name="f_geometry_column" type="field"/>
   <column width="-1" hidden="0" name="identifier" type="field"/>
   <column width="-1" hidden="0" name="title" type="field"/>
   <column width="-1" hidden="0" name="abstract" type="field"/>
   <column width="-1" hidden="0" name="geometry_type" type="field"/>
   <column width="-1" hidden="0" name="crs" type="field"/>
   <column width="-1" hidden="0" name="layer_type" type="field"/>
   <column width="-1" hidden="0" name="qmd" type="field"/>
   <column width="-1" hidden="0" name="owner" type="field"/>
   <column width="-1" hidden="0" name="update_time" type="field"/>
   <column width="-1" hidden="1" type="actions"/>
  </columns>
 </attributetableconfig>
 <conditionalstyles>
  <rowstyles/>
  <fieldstyles/>
 </conditionalstyles>
 <storedexpressions/>
 <editform tolerant="1"></editform>
 <editforminit/>
 <editforminitcodesource>0</editforminitcodesource>
 <editforminitfilepath></editforminitfilepath>
 <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
    geom = feature.geometry()
    control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
 <featformsuppress>0</featformsuppress>
 <editorlayout>generatedlayout</editorlayout>
 <editable>
  <field name="abstract" editable="1"/>
  <field name="crs" editable="1"/>
  <field name="f_geometry_column" editable="1"/>
  <field name="f_table_catalog" editable="1"/>
  <field name="f_table_name" editable="1"/>
  <field name="f_table_schema" editable="1"/>
  <field name="geometry_type" editable="1"/>
  <field name="id" editable="1"/>
  <field name="identifier" editable="1"/>
  <field name="layer_type" editable="1"/>
  <field name="owner" editable="1"/>
  <field name="qmd" editable="1"/>
  <field name="title" editable="1"/>
  <field name="update_time" editable="1"/>
 </editable>
 <labelOnTop>
  <field name="abstract" labelOnTop="0"/>
  <field name="crs" labelOnTop="0"/>
  <field name="f_geometry_column" labelOnTop="0"/>
  <field name="f_table_catalog" labelOnTop="0"/>
  <field name="f_table_name" labelOnTop="0"/>
  <field name="f_table_schema" labelOnTop="0"/>
  <field name="geometry_type" labelOnTop="0"/>
  <field name="id" labelOnTop="0"/>
  <field name="identifier" labelOnTop="0"/>
  <field name="layer_type" labelOnTop="0"/>
  <field name="owner" labelOnTop="0"/>
  <field name="qmd" labelOnTop="0"/>
  <field name="title" labelOnTop="0"/>
  <field name="update_time" labelOnTop="0"/>
 </labelOnTop>
 <reuseLastValue>
  <field reuseLastValue="0" name="abstract"/>
  <field reuseLastValue="0" name="crs"/>
  <field reuseLastValue="0" name="f_geometry_column"/>
  <field reuseLastValue="0" name="f_table_catalog"/>
  <field reuseLastValue="0" name="f_table_name"/>
  <field reuseLastValue="0" name="f_table_schema"/>
  <field reuseLastValue="0" name="geometry_type"/>
  <field reuseLastValue="0" name="id"/>
  <field reuseLastValue="0" name="identifier"/>
  <field reuseLastValue="0" name="layer_type"/>
  <field reuseLastValue="0" name="owner"/>
  <field reuseLastValue="0" name="qmd"/>
  <field reuseLastValue="0" name="title"/>
  <field reuseLastValue="0" name="update_time"/>
 </reuseLastValue>
 <dataDefinedFieldProperties/>
 <widgets/>
 <previewExpression>"f_table_name"</previewExpression>
 <mapTip enabled="1"></mapTip>
 <layerGeometryType>2</layerGeometryType>
</qgis>
', '<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" version="1.1.0" xmlns:se="http://www.opengis.net/se" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink">
 <NamedLayer>
  <se:Name>qgis_layer_metadata</se:Name>
  <UserStyle>
   <se:Name>qgis_layer_metadata</se:Name>
   <se:FeatureTypeStyle>
    <se:Rule>
     <se:Name>Single symbol</se:Name>
     <se:PolygonSymbolizer>
      <se:Fill>
       <se:SvgParameter name="fill">#b11daa</se:SvgParameter>
       <se:SvgParameter name="fill-opacity">0</se:SvgParameter>
      </se:Fill>
      <se:Stroke>
       <se:SvgParameter name="stroke">#b11daa</se:SvgParameter>
       <se:SvgParameter name="stroke-width">1</se:SvgParameter>
       <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
       <se:SvgParameter name="stroke-dasharray">4 2</se:SvgParameter>
      </se:Stroke>
     </se:PolygonSymbolizer>
    </se:Rule>
   </se:FeatureTypeStyle>
  </UserStyle>
 </NamedLayer>
</StyledLayerDescriptor>
', true, 'Wed Dec 11 15:51:47 2024', 'dominic.hinchley', NULL, '2024-12-11 15:51:47.809873', 'Polygon');
INSERT INTO qgis.layer_styles VALUES (4, 'land_terrier', 'land_admin', 'parcels', 'geom', 'parcels', '<!DOCTYPE qgis PUBLIC ''http://mrcc.com/qgis.dtd'' ''SYSTEM''>
<qgis simplifyAlgorithm="0" maxScale="0" simplifyLocal="1" simplifyDrawingHints="1" simplifyDrawingTol="1" simplifyMaxScale="1" readOnly="0" version="3.34.8-Prizren" hasScaleBasedVisibilityFlag="0" minScale="100000000" styleCategories="AllStyleCategories" labelsEnabled="1" symbologyReferenceScale="-1">
 <flags>
  <Identifiable>1</Identifiable>
  <Removable>1</Removable>
  <Searchable>1</Searchable>
  <Private>0</Private>
 </flags>
 <temporal enabled="0" startExpression="" durationUnit="min" accumulate="0" limitMode="0" mode="0" fixedDuration="0" endField="" durationField="area_ha" startField="valid_from" endExpression="">
  <fixedRange>
   <start></start>
   <end></end>
  </fixedRange>
 </temporal>
 <elevation showMarkerSymbolInSurfacePlots="0" type="IndividualFeatures" respectLayerSymbol="1" symbology="Line" extrusionEnabled="0" clamping="Terrain" binding="Centroid" zoffset="0" extrusion="0" zscale="1">
  <data-defined-properties>
   <Option type="Map">
    <Option type="QString" name="name" value=""/>
    <Option name="properties"/>
    <Option type="QString" name="type" value="collection"/>
   </Option>
  </data-defined-properties>
  <profileLineSymbol>
   <symbol type="line" name="" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" class="SimpleLine" id="{1ab0559e-d1fa-4332-8c2b-0fa96a06d3af}" locked="0" pass="0">
     <Option type="Map">
      <Option type="QString" name="align_dash_pattern" value="0"/>
      <Option type="QString" name="capstyle" value="square"/>
      <Option type="QString" name="customdash" value="5;2"/>
      <Option type="QString" name="customdash_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="customdash_unit" value="MM"/>
      <Option type="QString" name="dash_pattern_offset" value="0"/>
      <Option type="QString" name="dash_pattern_offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="dash_pattern_offset_unit" value="MM"/>
      <Option type="QString" name="draw_inside_polygon" value="0"/>
      <Option type="QString" name="joinstyle" value="bevel"/>
      <Option type="QString" name="line_color" value="196,60,57,255"/>
      <Option type="QString" name="line_style" value="solid"/>
      <Option type="QString" name="line_width" value="0.6"/>
      <Option type="QString" name="line_width_unit" value="MM"/>
      <Option type="QString" name="offset" value="0"/>
      <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="offset_unit" value="MM"/>
      <Option type="QString" name="ring_filter" value="0"/>
      <Option type="QString" name="trim_distance_end" value="0"/>
      <Option type="QString" name="trim_distance_end_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="trim_distance_end_unit" value="MM"/>
      <Option type="QString" name="trim_distance_start" value="0"/>
      <Option type="QString" name="trim_distance_start_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="trim_distance_start_unit" value="MM"/>
      <Option type="QString" name="tweak_dash_pattern_on_corners" value="0"/>
      <Option type="QString" name="use_custom_dash" value="0"/>
      <Option type="QString" name="width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileLineSymbol>
  <profileFillSymbol>
   <symbol type="fill" name="" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" class="SimpleFill" id="{c9c43197-45ef-413e-a2e3-109f0426c8e6}" locked="0" pass="0">
     <Option type="Map">
      <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="color" value="196,60,57,255"/>
      <Option type="QString" name="joinstyle" value="bevel"/>
      <Option type="QString" name="offset" value="0,0"/>
      <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="offset_unit" value="MM"/>
      <Option type="QString" name="outline_color" value="140,43,41,255"/>
      <Option type="QString" name="outline_style" value="solid"/>
      <Option type="QString" name="outline_width" value="0.2"/>
      <Option type="QString" name="outline_width_unit" value="MM"/>
      <Option type="QString" name="style" value="solid"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileFillSymbol>
  <profileMarkerSymbol>
   <symbol type="marker" name="" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" class="SimpleMarker" id="{0a44921d-a358-42f6-94a6-6097ef06fa95}" locked="0" pass="0">
     <Option type="Map">
      <Option type="QString" name="angle" value="0"/>
      <Option type="QString" name="cap_style" value="square"/>
      <Option type="QString" name="color" value="196,60,57,255"/>
      <Option type="QString" name="horizontal_anchor_point" value="1"/>
      <Option type="QString" name="joinstyle" value="bevel"/>
      <Option type="QString" name="name" value="diamond"/>
      <Option type="QString" name="offset" value="0,0"/>
      <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="offset_unit" value="MM"/>
      <Option type="QString" name="outline_color" value="140,43,41,255"/>
      <Option type="QString" name="outline_style" value="solid"/>
      <Option type="QString" name="outline_width" value="0.2"/>
      <Option type="QString" name="outline_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="outline_width_unit" value="MM"/>
      <Option type="QString" name="scale_method" value="diameter"/>
      <Option type="QString" name="size" value="3"/>
      <Option type="QString" name="size_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="size_unit" value="MM"/>
      <Option type="QString" name="vertical_anchor_point" value="1"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </profileMarkerSymbol>
 </elevation>
 <renderer-v2 enableorderby="0" type="singleSymbol" forceraster="0" symbollevels="0" referencescale="-1">
  <symbols>
   <symbol type="fill" name="0" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" class="SimpleFill" id="{237f81b0-b5b2-4316-9ace-63707cd50ba0}" locked="0" pass="0">
     <Option type="Map">
      <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="color" value="49,72,225,26"/>
      <Option type="QString" name="joinstyle" value="bevel"/>
      <Option type="QString" name="offset" value="0,0"/>
      <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="offset_unit" value="MM"/>
      <Option type="QString" name="outline_color" value="35,35,255,255"/>
      <Option type="QString" name="outline_style" value="solid"/>
      <Option type="QString" name="outline_width" value="0.4"/>
      <Option type="QString" name="outline_width_unit" value="MM"/>
      <Option type="QString" name="style" value="solid"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </symbols>
  <rotation/>
  <sizescale/>
 </renderer-v2>
 <selection mode="Default">
  <selectionColor invalid="1"/>
  <selectionSymbol>
   <symbol type="fill" name="" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
    <data_defined_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </data_defined_properties>
    <layer enabled="1" class="SimpleFill" id="{7fab4d7f-8188-413f-a362-52995c9fe5fc}" locked="0" pass="0">
     <Option type="Map">
      <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="color" value="0,0,255,255"/>
      <Option type="QString" name="joinstyle" value="bevel"/>
      <Option type="QString" name="offset" value="0,0"/>
      <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      <Option type="QString" name="offset_unit" value="MM"/>
      <Option type="QString" name="outline_color" value="35,35,35,255"/>
      <Option type="QString" name="outline_style" value="solid"/>
      <Option type="QString" name="outline_width" value="0.26"/>
      <Option type="QString" name="outline_width_unit" value="MM"/>
      <Option type="QString" name="style" value="solid"/>
     </Option>
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
    </layer>
   </symbol>
  </selectionSymbol>
 </selection>
 <labeling type="simple">
  <settings calloutType="simple">
   <text-style textOpacity="1" fontWordSpacing="0" legendString="Aa" fontKerning="1" textOrientation="horizontal" fontLetterSpacing="0" isExpression="0" multilineHeightUnit="Percentage" blendMode="0" fontItalic="0" allowHtml="0" textColor="50,50,50,255" fontWeight="50" fontSizeUnit="Point" multilineHeight="1" fontSize="10" fontStrikeout="0" namedStyle="Regular" fontFamily="Open Sans" previewBkgrdColor="255,255,255,255" useSubstitutions="0" forcedBold="0" fontUnderline="0" forcedItalic="0" fontSizeMapUnitScale="3x:0,0,0,0,0,0" capitalization="0" fieldName="parcel_alias">
    <families/>
    <text-buffer bufferOpacity="1" bufferNoFill="1" bufferSizeMapUnitScale="3x:0,0,0,0,0,0" bufferJoinStyle="128" bufferSizeUnits="MM" bufferColor="250,250,250,255" bufferBlendMode="0" bufferSize="1" bufferDraw="0"/>
    <text-mask maskedSymbolLayers="" maskType="0" maskJoinStyle="128" maskEnabled="0" maskOpacity="1" maskSizeMapUnitScale="3x:0,0,0,0,0,0" maskSizeUnits="MM" maskSize="0"/>
    <background shapeJoinStyle="64" shapeRotation="0" shapeRotationType="0" shapeSVGFile="" shapeOffsetMapUnitScale="3x:0,0,0,0,0,0" shapeSizeType="0" shapeOpacity="1" shapeOffsetX="0" shapeBorderColor="128,128,128,255" shapeRadiiUnit="Point" shapeSizeY="0" shapeRadiiMapUnitScale="3x:0,0,0,0,0,0" shapeBorderWidthMapUnitScale="3x:0,0,0,0,0,0" shapeDraw="0" shapeSizeMapUnitScale="3x:0,0,0,0,0,0" shapeOffsetUnit="Point" shapeRadiiY="0" shapeBorderWidth="0" shapeBlendMode="0" shapeOffsetY="0" shapeRadiiX="0" shapeFillColor="255,255,255,255" shapeBorderWidthUnit="Point" shapeSizeUnit="Point" shapeSizeX="0" shapeType="0">
     <symbol type="marker" name="markerSymbol" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" name="name" value=""/>
        <Option name="properties"/>
        <Option type="QString" name="type" value="collection"/>
       </Option>
      </data_defined_properties>
      <layer enabled="1" class="SimpleMarker" id="" locked="0" pass="0">
       <Option type="Map">
        <Option type="QString" name="angle" value="0"/>
        <Option type="QString" name="cap_style" value="square"/>
        <Option type="QString" name="color" value="229,182,54,255"/>
        <Option type="QString" name="horizontal_anchor_point" value="1"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="name" value="circle"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="35,35,35,255"/>
        <Option type="QString" name="outline_style" value="solid"/>
        <Option type="QString" name="outline_width" value="0"/>
        <Option type="QString" name="outline_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="outline_width_unit" value="MM"/>
        <Option type="QString" name="scale_method" value="diameter"/>
        <Option type="QString" name="size" value="2"/>
        <Option type="QString" name="size_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="size_unit" value="MM"/>
        <Option type="QString" name="vertical_anchor_point" value="1"/>
       </Option>
       <data_defined_properties>
        <Option type="Map">
         <Option type="QString" name="name" value=""/>
         <Option name="properties"/>
         <Option type="QString" name="type" value="collection"/>
        </Option>
       </data_defined_properties>
      </layer>
     </symbol>
     <symbol type="fill" name="fillSymbol" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" name="name" value=""/>
        <Option name="properties"/>
        <Option type="QString" name="type" value="collection"/>
       </Option>
      </data_defined_properties>
      <layer enabled="1" class="SimpleFill" id="" locked="0" pass="0">
       <Option type="Map">
        <Option type="QString" name="border_width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="color" value="255,255,255,255"/>
        <Option type="QString" name="joinstyle" value="bevel"/>
        <Option type="QString" name="offset" value="0,0"/>
        <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
        <Option type="QString" name="offset_unit" value="MM"/>
        <Option type="QString" name="outline_color" value="128,128,128,255"/>
        <Option type="QString" name="outline_style" value="no"/>
        <Option type="QString" name="outline_width" value="0"/>
        <Option type="QString" name="outline_width_unit" value="Point"/>
        <Option type="QString" name="style" value="solid"/>
       </Option>
       <data_defined_properties>
        <Option type="Map">
         <Option type="QString" name="name" value=""/>
         <Option name="properties"/>
         <Option type="QString" name="type" value="collection"/>
        </Option>
       </data_defined_properties>
      </layer>
     </symbol>
    </background>
    <shadow shadowUnder="0" shadowColor="0,0,0,255" shadowRadiusMapUnitScale="3x:0,0,0,0,0,0" shadowRadiusAlphaOnly="0" shadowOffsetAngle="135" shadowOffsetGlobal="1" shadowRadiusUnit="MM" shadowDraw="0" shadowRadius="1.5" shadowOpacity="0.69999999999999996" shadowOffsetUnit="MM" shadowScale="100" shadowBlendMode="6" shadowOffsetDist="1" shadowOffsetMapUnitScale="3x:0,0,0,0,0,0"/>
    <dd_properties>
     <Option type="Map">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
    </dd_properties>
    <substitutions/>
   </text-style>
   <text-format wrapChar="" leftDirectionSymbol="&lt;" rightDirectionSymbol=">" addDirectionSymbol="0" autoWrapLength="0" placeDirectionSymbol="0" plussign="0" decimals="3" multilineAlign="3" formatNumbers="0" useMaxLineLengthForAutoWrap="1" reverseDirectionSymbol="0"/>
   <placement centroidInside="0" xOffset="0" polygonPlacementFlags="2" allowDegraded="0" geometryGeneratorEnabled="0" maxCurvedCharAngleIn="25" fitInPolygonOnly="0" repeatDistanceMapUnitScale="3x:0,0,0,0,0,0" rotationAngle="0" repeatDistance="0" lineAnchorClipping="0" distMapUnitScale="3x:0,0,0,0,0,0" centroidWhole="0" offsetType="0" dist="0" placementFlags="10" lineAnchorPercent="0.5" placement="0" geometryGeneratorType="PointGeometry" repeatDistanceUnits="MM" predefinedPositionOrder="TR,TL,BR,BL,R,L,TSR,BSR" overrunDistanceMapUnitScale="3x:0,0,0,0,0,0" distUnits="MM" preserveRotation="1" geometryGenerator="" lineAnchorTextPoint="FollowPlacement" overlapHandling="PreventOverlap" labelOffsetMapUnitScale="3x:0,0,0,0,0,0" overrunDistance="0" layerType="PolygonGeometry" offsetUnits="MM" overrunDistanceUnit="MM" maxCurvedCharAngleOut="-25" priority="5" lineAnchorType="0" yOffset="0" quadOffset="4" rotationUnit="AngleDegrees"/>
   <rendering fontMaxPixelSize="10000" maxNumLabels="2000" scaleMin="0" scaleMax="50000" drawLabels="1" minFeatureSize="0" fontLimitPixelSize="0" zIndex="0" upsidedownLabels="0" mergeLines="0" obstacleFactor="1" obstacle="1" limitNumLabels="0" obstacleType="1" fontMinPixelSize="3" labelPerPart="0" unplacedVisibility="0" scaleVisibility="1"/>
   <dd_properties>
    <Option type="Map">
     <Option type="QString" name="name" value=""/>
     <Option name="properties"/>
     <Option type="QString" name="type" value="collection"/>
    </Option>
   </dd_properties>
   <callout type="simple">
    <Option type="Map">
     <Option type="QString" name="anchorPoint" value="pole_of_inaccessibility"/>
     <Option type="int" name="blendMode" value="0"/>
     <Option type="Map" name="ddProperties">
      <Option type="QString" name="name" value=""/>
      <Option name="properties"/>
      <Option type="QString" name="type" value="collection"/>
     </Option>
     <Option type="bool" name="drawToAllParts" value="false"/>
     <Option type="QString" name="enabled" value="0"/>
     <Option type="QString" name="labelAnchorPoint" value="point_on_exterior"/>
     <Option type="QString" name="lineSymbol" value="&lt;symbol type=&quot;line&quot; name=&quot;symbol&quot; force_rhr=&quot;0&quot; alpha=&quot;1&quot; is_animated=&quot;0&quot; frame_rate=&quot;10&quot; clip_to_extent=&quot;1&quot;>&lt;data_defined_properties>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; name=&quot;name&quot; value=&quot;&quot;/>&lt;Option name=&quot;properties&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;type&quot; value=&quot;collection&quot;/>&lt;/Option>&lt;/data_defined_properties>&lt;layer enabled=&quot;1&quot; class=&quot;SimpleLine&quot; id=&quot;{aecb53ca-e54b-4f84-aed0-c02b22748dc9}&quot; locked=&quot;0&quot; pass=&quot;0&quot;>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; name=&quot;align_dash_pattern&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;capstyle&quot; value=&quot;square&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;customdash&quot; value=&quot;5;2&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;customdash_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;customdash_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;dash_pattern_offset&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;dash_pattern_offset_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;dash_pattern_offset_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;draw_inside_polygon&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;joinstyle&quot; value=&quot;bevel&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;line_color&quot; value=&quot;60,60,60,255&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;line_style&quot; value=&quot;solid&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;line_width&quot; value=&quot;0.3&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;line_width_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;offset&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;offset_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;offset_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;ring_filter&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_end&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_end_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_end_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_start&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_start_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;trim_distance_start_unit&quot; value=&quot;MM&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;tweak_dash_pattern_on_corners&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;use_custom_dash&quot; value=&quot;0&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;width_map_unit_scale&quot; value=&quot;3x:0,0,0,0,0,0&quot;/>&lt;/Option>&lt;data_defined_properties>&lt;Option type=&quot;Map&quot;>&lt;Option type=&quot;QString&quot; name=&quot;name&quot; value=&quot;&quot;/>&lt;Option name=&quot;properties&quot;/>&lt;Option type=&quot;QString&quot; name=&quot;type&quot; value=&quot;collection&quot;/>&lt;/Option>&lt;/data_defined_properties>&lt;/layer>&lt;/symbol>"/>
     <Option type="double" name="minLength" value="0"/>
     <Option type="QString" name="minLengthMapUnitScale" value="3x:0,0,0,0,0,0"/>
     <Option type="QString" name="minLengthUnit" value="MM"/>
     <Option type="double" name="offsetFromAnchor" value="0"/>
     <Option type="QString" name="offsetFromAnchorMapUnitScale" value="3x:0,0,0,0,0,0"/>
     <Option type="QString" name="offsetFromAnchorUnit" value="MM"/>
     <Option type="double" name="offsetFromLabel" value="0"/>
     <Option type="QString" name="offsetFromLabelMapUnitScale" value="3x:0,0,0,0,0,0"/>
     <Option type="QString" name="offsetFromLabelUnit" value="MM"/>
    </Option>
   </callout>
  </settings>
 </labeling>
 <customproperties>
  <Option type="Map">
   <Option type="QString" name="QFieldSync/action" value="offline"/>
   <Option type="QString" name="QFieldSync/attachment_naming" value="{}"/>
   <Option type="QString" name="QFieldSync/cloud_action" value="offline"/>
   <Option type="QString" name="QFieldSync/geometry_locked_expression" value=""/>
   <Option type="QString" name="QFieldSync/photo_naming" value="{}"/>
   <Option type="QString" name="QFieldSync/relationship_maximum_visible" value="{&quot;tenure_9a790ae4_c8d2_4d5c_afb5_3ab7ba90c7c5_parcel_id_parcels_c51f3997_8de2_4c43_b598_99f9e6534a61_parcel_id&quot;: 4, &quot;freehold_tenure_e377b1f2_dcc4_4113_97fa_8b7a04399ef9_parcel_uuid_parcels_c782dc1d_338d_46ee_9b75_7f1eede0cfd1_parcel_uuid&quot;: 4}"/>
   <Option type="int" name="QFieldSync/tracking_distance_requirement_minimum_meters" value="30"/>
   <Option type="int" name="QFieldSync/tracking_erroneous_distance_safeguard_maximum_meters" value="1"/>
   <Option type="int" name="QFieldSync/tracking_measurement_type" value="0"/>
   <Option type="int" name="QFieldSync/tracking_time_requirement_interval_seconds" value="30"/>
   <Option type="int" name="QFieldSync/value_map_button_interface_threshold" value="0"/>
   <Option type="List" name="dualview/previewExpressions">
    <Option type="QString" value="COALESCE( &quot;site_id&quot;, '''' ) || coalesce('': '' || parcel_alias, '''') "/>
   </Option>
   <Option type="int" name="embeddedWidgets/count" value="0"/>
   <Option name="variableNames"/>
   <Option name="variableValues"/>
  </Option>
 </customproperties>
 <blendMode>0</blendMode>
 <featureBlendMode>0</featureBlendMode>
 <layerOpacity>1</layerOpacity>
 <SingleCategoryDiagramRenderer diagramType="Histogram" attributeLegend="1">
  <DiagramCategory barWidth="5" width="15" penColor="#000000" lineSizeScale="3x:0,0,0,0,0,0" opacity="1" maxScaleDenominator="1e+08" minimumSize="0" spacingUnitScale="3x:0,0,0,0,0,0" labelPlacementMethod="XHeight" minScaleDenominator="0" backgroundAlpha="255" spacingUnit="MM" sizeScale="3x:0,0,0,0,0,0" sizeType="MM" enabled="0" direction="0" lineSizeType="MM" scaleDependency="Area" height="15" backgroundColor="#ffffff" penAlpha="255" showAxis="1" rotationOffset="270" spacing="5" penWidth="0" scaleBasedVisibility="0" diagramOrientation="Up">
   <fontProperties description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   <attribute field="" color="#000000" label="" colorOpacity="1"/>
   <axisSymbol>
    <symbol type="line" name="" force_rhr="0" alpha="1" is_animated="0" frame_rate="10" clip_to_extent="1">
     <data_defined_properties>
      <Option type="Map">
       <Option type="QString" name="name" value=""/>
       <Option name="properties"/>
       <Option type="QString" name="type" value="collection"/>
      </Option>
     </data_defined_properties>
     <layer enabled="1" class="SimpleLine" id="{c765e9c7-3717-46c2-be19-ae0ca3b40f50}" locked="0" pass="0">
      <Option type="Map">
       <Option type="QString" name="align_dash_pattern" value="0"/>
       <Option type="QString" name="capstyle" value="square"/>
       <Option type="QString" name="customdash" value="5;2"/>
       <Option type="QString" name="customdash_map_unit_scale" value="3x:0,0,0,0,0,0"/>
       <Option type="QString" name="customdash_unit" value="MM"/>
       <Option type="QString" name="dash_pattern_offset" value="0"/>
       <Option type="QString" name="dash_pattern_offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
       <Option type="QString" name="dash_pattern_offset_unit" value="MM"/>
       <Option type="QString" name="draw_inside_polygon" value="0"/>
       <Option type="QString" name="joinstyle" value="bevel"/>
       <Option type="QString" name="line_color" value="35,35,35,255"/>
       <Option type="QString" name="line_style" value="solid"/>
       <Option type="QString" name="line_width" value="0.26"/>
       <Option type="QString" name="line_width_unit" value="MM"/>
       <Option type="QString" name="offset" value="0"/>
       <Option type="QString" name="offset_map_unit_scale" value="3x:0,0,0,0,0,0"/>
       <Option type="QString" name="offset_unit" value="MM"/>
       <Option type="QString" name="ring_filter" value="0"/>
       <Option type="QString" name="trim_distance_end" value="0"/>
       <Option type="QString" name="trim_distance_end_map_unit_scale" value="3x:0,0,0,0,0,0"/>
       <Option type="QString" name="trim_distance_end_unit" value="MM"/>
       <Option type="QString" name="trim_distance_start" value="0"/>
       <Option type="QString" name="trim_distance_start_map_unit_scale" value="3x:0,0,0,0,0,0"/>
       <Option type="QString" name="trim_distance_start_unit" value="MM"/>
       <Option type="QString" name="tweak_dash_pattern_on_corners" value="0"/>
       <Option type="QString" name="use_custom_dash" value="0"/>
       <Option type="QString" name="width_map_unit_scale" value="3x:0,0,0,0,0,0"/>
      </Option>
      <data_defined_properties>
       <Option type="Map">
        <Option type="QString" name="name" value=""/>
        <Option name="properties"/>
        <Option type="QString" name="type" value="collection"/>
       </Option>
      </data_defined_properties>
     </layer>
    </symbol>
   </axisSymbol>
  </DiagramCategory>
 </SingleCategoryDiagramRenderer>
 <DiagramLayerSettings priority="0" obstacle="0" dist="0" zIndex="0" placement="1" showAll="1" linePlacementFlags="18">
  <properties>
   <Option type="Map">
    <Option type="QString" name="name" value=""/>
    <Option name="properties"/>
    <Option type="QString" name="type" value="collection"/>
   </Option>
  </properties>
 </DiagramLayerSettings>
 <geometryOptions removeDuplicateNodes="0" geometryPrecision="0">
  <activeChecks/>
  <checkConfiguration type="Map">
   <Option type="Map" name="QgsGeometryGapCheck">
    <Option type="double" name="allowedGapsBuffer" value="0"/>
    <Option type="bool" name="allowedGapsEnabled" value="false"/>
    <Option type="QString" name="allowedGapsLayer" value=""/>
   </Option>
  </checkConfiguration>
 </geometryOptions>
 <legend type="default-vector" showLabelLegend="0"/>
 <referencedLayers>
  <relation name="parcels_site_id_fkey" referencedLayer="sites_43675a7c_6dbd_4d2c_8402_ba42d3a69217" referencingLayer="parcels_c782dc1d_338d_46ee_9b75_7f1eede0cfd1" dataSource="dbname=''ywt_gis'' host=192.168.0.117 port=5432 key=''site_id'' checkPrimaryKeyUnicity=''1'' table=&quot;land_admin&quot;.&quot;sites&quot;" id="parcels_c782dc1d_338d_46ee_9b75_7f1eede0cfd1_site_id_sites_43675a7c_6dbd_4d2c_8402_ba42d3a69217_site_id" strength="Association" providerKey="postgres" layerId="sites_43675a7c_6dbd_4d2c_8402_ba42d3a69217" layerName="sites">
   <fieldRef referencingField="site_id" referencedField="site_id"/>
  </relation>
 </referencedLayers>
 <fieldConfiguration>
  <field name="parcel_uuid" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="false"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="site_id" configurationFlags="NoFlag">
   <editWidget type="ValueRelation">
    <config>
     <Option type="Map">
      <Option type="bool" name="AllowMulti" value="false"/>
      <Option type="bool" name="AllowNull" value="false"/>
      <Option type="QString" name="Description" value="&quot;site_name&quot;"/>
      <Option type="QString" name="FilterExpression" value=""/>
      <Option type="QString" name="Key" value="site_id"/>
      <Option type="QString" name="Layer" value="sites_43675a7c_6dbd_4d2c_8402_ba42d3a69217"/>
      <Option type="QString" name="LayerName" value="sites"/>
      <Option type="QString" name="LayerProviderName" value="postgres"/>
      <Option type="QString" name="LayerSource" value="dbname=''ywt_gis'' host=192.168.0.117 port=5432 key=''site_id'' checkPrimaryKeyUnicity=''1'' table=&quot;land_admin&quot;.&quot;sites&quot;"/>
      <Option type="int" name="NofColumns" value="1"/>
      <Option type="bool" name="OrderByValue" value="false"/>
      <Option type="bool" name="UseCompleter" value="false"/>
      <Option type="QString" name="Value" value="site_name"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="parcel_alias" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="false"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="boundary_type_code" configurationFlags="NoFlag">
   <editWidget type="ValueRelation">
    <config>
     <Option type="Map">
      <Option type="bool" name="AllowMulti" value="false"/>
      <Option type="bool" name="AllowNull" value="true"/>
      <Option type="QString" name="Description" value="&quot;description&quot;"/>
      <Option type="QString" name="FilterExpression" value=""/>
      <Option type="QString" name="Key" value="boundary_type_code"/>
      <Option type="QString" name="Layer" value="dom_boundary_types_f59e3362_9e3d_42b8_8a01_af861e91d79c"/>
      <Option type="QString" name="LayerName" value="dom_boundary_types"/>
      <Option type="QString" name="LayerProviderName" value="postgres"/>
      <Option type="QString" name="LayerSource" value="dbname=''ywt_gis'' host=192.168.0.117 port=5432 key=''boundary_type_code'' checkPrimaryKeyUnicity=''1'' table=&quot;domains&quot;.&quot;dom_boundary_types&quot;"/>
      <Option type="int" name="NofColumns" value="1"/>
      <Option type="bool" name="OrderByValue" value="false"/>
      <Option type="bool" name="UseCompleter" value="false"/>
      <Option type="QString" name="Value" value="boundary_type"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="access_status_code" configurationFlags="NoFlag">
   <editWidget type="ValueRelation">
    <config>
     <Option type="Map">
      <Option type="bool" name="AllowMulti" value="false"/>
      <Option type="bool" name="AllowNull" value="true"/>
      <Option type="QString" name="Description" value="&quot;description&quot;"/>
      <Option type="QString" name="FilterExpression" value=""/>
      <Option type="QString" name="Key" value="access_status_code"/>
      <Option type="QString" name="Layer" value="dom_access_status_39cb4635_0b23_4cf7_9ef2_ea076ae82e32"/>
      <Option type="QString" name="LayerName" value="dom_access_status"/>
      <Option type="QString" name="LayerProviderName" value="postgres"/>
      <Option type="QString" name="LayerSource" value="dbname=''ywt_gis'' host=192.168.0.117 port=5432 key=''access_status_code'' checkPrimaryKeyUnicity=''1'' table=&quot;domains&quot;.&quot;dom_access_status&quot;"/>
      <Option type="int" name="NofColumns" value="1"/>
      <Option type="bool" name="OrderByValue" value="false"/>
      <Option type="bool" name="UseCompleter" value="false"/>
      <Option type="QString" name="Value" value="access_status"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="notes" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="true"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="area_ha" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="false"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="valid_from" configurationFlags="NoFlag">
   <editWidget type="DateTime">
    <config>
     <Option type="Map">
      <Option type="bool" name="allow_null" value="true"/>
      <Option type="bool" name="calendar_popup" value="true"/>
      <Option type="QString" name="display_format" value="dd/MM/yyyy"/>
      <Option type="QString" name="field_format" value="yyyy-MM-dd"/>
      <Option type="bool" name="field_format_overwrite" value="false"/>
      <Option type="bool" name="field_iso_format" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="valid_to" configurationFlags="NoFlag">
   <editWidget type="DateTime">
    <config>
     <Option type="Map">
      <Option type="bool" name="allow_null" value="true"/>
      <Option type="bool" name="calendar_popup" value="true"/>
      <Option type="QString" name="display_format" value="dd/MM/yyyy"/>
      <Option type="QString" name="field_format" value="yyyy-MM-dd"/>
      <Option type="bool" name="field_format_overwrite" value="false"/>
      <Option type="bool" name="field_iso_format" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="updated_at" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="false"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
  <field name="updated_by" configurationFlags="NoFlag">
   <editWidget type="TextEdit">
    <config>
     <Option type="Map">
      <Option type="bool" name="IsMultiline" value="false"/>
      <Option type="bool" name="UseHtml" value="false"/>
     </Option>
    </config>
   </editWidget>
  </field>
 </fieldConfiguration>
 <aliases>
  <alias field="parcel_uuid" name="" index="0"/>
  <alias field="site_id" name="" index="1"/>
  <alias field="parcel_alias" name="" index="2"/>
  <alias field="boundary_type_code" name="" index="3"/>
  <alias field="access_status_code" name="" index="4"/>
  <alias field="notes" name="" index="5"/>
  <alias field="area_ha" name="" index="6"/>
  <alias field="valid_from" name="" index="7"/>
  <alias field="valid_to" name="" index="8"/>
  <alias field="updated_at" name="" index="9"/>
  <alias field="updated_by" name="" index="10"/>
 </aliases>
 <splitPolicies>
  <policy field="parcel_uuid" policy="Duplicate"/>
  <policy field="site_id" policy="Duplicate"/>
  <policy field="parcel_alias" policy="Duplicate"/>
  <policy field="boundary_type_code" policy="Duplicate"/>
  <policy field="access_status_code" policy="Duplicate"/>
  <policy field="notes" policy="Duplicate"/>
  <policy field="area_ha" policy="Duplicate"/>
  <policy field="valid_from" policy="Duplicate"/>
  <policy field="valid_to" policy="Duplicate"/>
  <policy field="updated_at" policy="Duplicate"/>
  <policy field="updated_by" policy="Duplicate"/>
 </splitPolicies>
 <defaults>
  <default field="parcel_uuid" expression="" applyOnUpdate="0"/>
  <default field="site_id" expression="" applyOnUpdate="0"/>
  <default field="parcel_alias" expression="" applyOnUpdate="0"/>
  <default field="boundary_type_code" expression="" applyOnUpdate="0"/>
  <default field="access_status_code" expression="" applyOnUpdate="0"/>
  <default field="notes" expression="" applyOnUpdate="0"/>
  <default field="area_ha" expression="" applyOnUpdate="0"/>
  <default field="valid_from" expression="" applyOnUpdate="0"/>
  <default field="valid_to" expression="" applyOnUpdate="0"/>
  <default field="updated_at" expression="" applyOnUpdate="0"/>
  <default field="updated_by" expression="" applyOnUpdate="0"/>
 </defaults>
 <constraints>
  <constraint unique_strength="1" field="parcel_uuid" exp_strength="0" constraints="3" notnull_strength="1"/>
  <constraint unique_strength="0" field="site_id" exp_strength="0" constraints="1" notnull_strength="1"/>
  <constraint unique_strength="0" field="parcel_alias" exp_strength="0" constraints="1" notnull_strength="1"/>
  <constraint unique_strength="0" field="boundary_type_code" exp_strength="0" constraints="1" notnull_strength="1"/>
  <constraint unique_strength="0" field="access_status_code" exp_strength="0" constraints="1" notnull_strength="1"/>
  <constraint unique_strength="0" field="notes" exp_strength="0" constraints="0" notnull_strength="0"/>
  <constraint unique_strength="0" field="area_ha" exp_strength="0" constraints="0" notnull_strength="0"/>
  <constraint unique_strength="0" field="valid_from" exp_strength="0" constraints="0" notnull_strength="0"/>
  <constraint unique_strength="0" field="valid_to" exp_strength="0" constraints="0" notnull_strength="0"/>
  <constraint unique_strength="0" field="updated_at" exp_strength="0" constraints="1" notnull_strength="1"/>
  <constraint unique_strength="0" field="updated_by" exp_strength="0" constraints="1" notnull_strength="1"/>
 </constraints>
 <constraintExpressions>
  <constraint field="parcel_uuid" desc="" exp=""/>
  <constraint field="site_id" desc="" exp=""/>
  <constraint field="parcel_alias" desc="" exp=""/>
  <constraint field="boundary_type_code" desc="" exp=""/>
  <constraint field="access_status_code" desc="" exp=""/>
  <constraint field="notes" desc="" exp=""/>
  <constraint field="area_ha" desc="" exp=""/>
  <constraint field="valid_from" desc="" exp=""/>
  <constraint field="valid_to" desc="" exp=""/>
  <constraint field="updated_at" desc="" exp=""/>
  <constraint field="updated_by" desc="" exp=""/>
 </constraintExpressions>
 <expressionfields/>
 <attributeactions>
  <defaultAction value="{00000000-0000-0000-0000-000000000000}" key="Canvas"/>
 </attributeactions>
 <attributetableconfig actionWidgetStyle="dropDown" sortExpression="&quot;updated_by&quot;" sortOrder="0">
  <columns>
   <column type="field" name="site_id" width="354" hidden="0"/>
   <column type="field" name="area_ha" width="-1" hidden="0"/>
   <column type="field" name="notes" width="-1" hidden="0"/>
   <column type="field" name="parcel_uuid" width="-1" hidden="0"/>
   <column type="field" name="parcel_alias" width="-1" hidden="0"/>
   <column type="field" name="boundary_type_code" width="-1" hidden="0"/>
   <column type="field" name="access_status_code" width="-1" hidden="0"/>
   <column type="field" name="valid_from" width="-1" hidden="0"/>
   <column type="field" name="valid_to" width="-1" hidden="0"/>
   <column type="field" name="updated_at" width="152" hidden="0"/>
   <column type="field" name="updated_by" width="-1" hidden="0"/>
   <column type="actions" width="-1" hidden="1"/>
  </columns>
 </attributetableconfig>
 <conditionalstyles>
  <rowstyles>
   <style name="geom not checked" background_color="#fb9a99" background_color_alpha="255" rule="NOT geom_checked">
    <font description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   </style>
  </rowstyles>
  <fieldstyles/>
 </conditionalstyles>
 <storedexpressions/>
 <editform tolerant="1"></editform>
 <editforminit/>
 <editforminitcodesource>0</editforminitcodesource>
 <editforminitfilepath></editforminitfilepath>
 <editforminitcode><![CDATA[# -*- coding: utf-8 -*-
"""
QGIS forms can have a Python function that is called when the form is
opened.

Use this function to add extra logic to your forms.

Enter the name of the function in the "Python Init function"
field.
An example follows:
"""
from qgis.PyQt.QtWidgets import QWidget

def my_form_open(dialog, layer, feature):
    geom = feature.geometry()
    control = dialog.findChild(QWidget, "MyLineEdit")
]]></editforminitcode>
 <featformsuppress>0</featformsuppress>
 <editorlayout>tablayout</editorlayout>
 <attributeEditorForm>
  <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
   <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
  </labelStyle>
  <attributeEditorContainer columnCount="1" type="Tab" name="General" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="1" groupBox="0" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
   <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
    <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   </labelStyle>
   <attributeEditorContainer columnCount="2" type="GroupBox" name="Parcel" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="1" groupBox="1" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
    <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
     <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
    </labelStyle>
    <attributeEditorField name="site_id" horizontalStretch="0" index="1" showLabel="1" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
    </attributeEditorField>
    <attributeEditorField name="parcel_alias" horizontalStretch="0" index="2" showLabel="1" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
    </attributeEditorField>
    <attributeEditorField name="boundary_type_code" horizontalStretch="0" index="3" showLabel="1" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
    </attributeEditorField>
    <attributeEditorField name="access_status_code" horizontalStretch="0" index="4" showLabel="1" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
    </attributeEditorField>
    <attributeEditorField name="area_ha" horizontalStretch="0" index="6" showLabel="1" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
    </attributeEditorField>
    <attributeEditorContainer columnCount="1" type="Row" name="History Tracking" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="0" groupBox="0" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
     <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
      <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
     </labelStyle>
     <attributeEditorField name="valid_from" horizontalStretch="0" index="7" showLabel="1" verticalStretch="0">
      <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
       <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
      </labelStyle>
     </attributeEditorField>
     <attributeEditorField name="valid_to" horizontalStretch="0" index="8" showLabel="1" verticalStretch="0">
      <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
       <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
      </labelStyle>
     </attributeEditorField>
    </attributeEditorContainer>
   </attributeEditorContainer>
  </attributeEditorContainer>
  <attributeEditorContainer columnCount="1" type="Tab" name="File" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="1" groupBox="0" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
   <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
    <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   </labelStyle>
   <attributeEditorField name="notes" horizontalStretch="0" index="5" showLabel="1" verticalStretch="0">
    <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
     <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
    </labelStyle>
   </attributeEditorField>
  </attributeEditorContainer>
  <attributeEditorContainer columnCount="1" type="Tab" name="Metadata" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="1" groupBox="0" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
   <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
    <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   </labelStyle>
   <attributeEditorField name="updated_by" horizontalStretch="0" index="10" showLabel="1" verticalStretch="0">
    <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
     <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
    </labelStyle>
   </attributeEditorField>
   <attributeEditorField name="updated_at" horizontalStretch="0" index="9" showLabel="1" verticalStretch="0">
    <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
     <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
    </labelStyle>
   </attributeEditorField>
  </attributeEditorContainer>
  <attributeEditorContainer columnCount="1" type="GroupBox" name="Tenure" collapsed="0" horizontalStretch="0" collapsedExpressionEnabled="0" visibilityExpression="" showLabel="1" groupBox="1" collapsedExpression="" visibilityExpressionEnabled="0" verticalStretch="0">
   <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
    <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
   </labelStyle>
   <attributeEditorRelation relationWidgetTypeId="relation_editor" nmRelationId="" name="freehold_tenure_e377b1f2_dcc4_4113_97fa_8b7a04399ef9_parcel_uuid_parcels_c782dc1d_338d_46ee_9b75_7f1eede0cfd1_parcel_uuid" forceSuppressFormPopup="0" horizontalStretch="0" relation="freehold_tenure_e377b1f2_dcc4_4113_97fa_8b7a04399ef9_parcel_uuid_parcels_c782dc1d_338d_46ee_9b75_7f1eede0cfd1_parcel_uuid" label="Land Ownership" showLabel="1" verticalStretch="0">
    <labelStyle overrideLabelColor="0" labelColor="0,0,0,255" overrideLabelFont="0">
     <labelFont description="MS Shell Dlg 2,8.3,-1,5,50,0,0,0,0,0" strikethrough="0" underline="0" italic="0" style="" bold="0"/>
    </labelStyle>
    <editor_configuration type="Map">
     <Option type="bool" name="allow_add_child_feature_with_no_geometry" value="false"/>
     <Option type="QString" name="buttons" value="SaveChildEdits|AddChildFeature|DeleteChildFeature"/>
     <Option type="bool" name="show_first_feature" value="true"/>
    </editor_configuration>
   </attributeEditorRelation>
  </attributeEditorContainer>
 </attributeEditorForm>
 <editable>
  <field name="access_status_code" editable="1"/>
  <field name="area_ha" editable="0"/>
  <field name="boundary_quality" editable="1"/>
  <field name="boundary_type_code" editable="1"/>
  <field name="date_modified" editable="0"/>
  <field name="geom_checked" editable="1"/>
  <field name="land_registry" editable="1"/>
  <field name="modified_by" editable="0"/>
  <field name="notes" editable="1"/>
  <field name="parcel_alias" editable="1"/>
  <field name="parcel_class" editable="1"/>
  <field name="parcel_id" editable="0"/>
  <field name="parcel_name" editable="1"/>
  <field name="parcel_type" editable="1"/>
  <field name="parcel_uuid" editable="1"/>
  <field name="site_id" editable="1"/>
  <field name="updated_at" editable="1"/>
  <field name="updated_by" editable="1"/>
  <field name="valid_from" editable="0"/>
  <field name="valid_to" editable="1"/>
 </editable>
 <labelOnTop>
  <field labelOnTop="0" name="access_status_code"/>
  <field labelOnTop="0" name="area_ha"/>
  <field labelOnTop="0" name="boundary_quality"/>
  <field labelOnTop="0" name="boundary_type_code"/>
  <field labelOnTop="0" name="date_modified"/>
  <field labelOnTop="0" name="geom_checked"/>
  <field labelOnTop="0" name="land_registry"/>
  <field labelOnTop="0" name="modified_by"/>
  <field labelOnTop="0" name="notes"/>
  <field labelOnTop="0" name="parcel_alias"/>
  <field labelOnTop="0" name="parcel_class"/>
  <field labelOnTop="0" name="parcel_id"/>
  <field labelOnTop="0" name="parcel_name"/>
  <field labelOnTop="0" name="parcel_type"/>
  <field labelOnTop="0" name="parcel_uuid"/>
  <field labelOnTop="0" name="site_id"/>
  <field labelOnTop="0" name="updated_at"/>
  <field labelOnTop="0" name="updated_by"/>
  <field labelOnTop="0" name="valid_from"/>
  <field labelOnTop="0" name="valid_to"/>
 </labelOnTop>
 <reuseLastValue>
  <field name="access_status_code" reuseLastValue="0"/>
  <field name="area_ha" reuseLastValue="0"/>
  <field name="boundary_quality" reuseLastValue="0"/>
  <field name="boundary_type_code" reuseLastValue="0"/>
  <field name="date_modified" reuseLastValue="0"/>
  <field name="geom_checked" reuseLastValue="0"/>
  <field name="land_registry" reuseLastValue="0"/>
  <field name="modified_by" reuseLastValue="0"/>
  <field name="notes" reuseLastValue="0"/>
  <field name="parcel_alias" reuseLastValue="0"/>
  <field name="parcel_class" reuseLastValue="0"/>
  <field name="parcel_id" reuseLastValue="0"/>
  <field name="parcel_name" reuseLastValue="0"/>
  <field name="parcel_type" reuseLastValue="0"/>
  <field name="parcel_uuid" reuseLastValue="0"/>
  <field name="site_id" reuseLastValue="0"/>
  <field name="updated_at" reuseLastValue="0"/>
  <field name="updated_by" reuseLastValue="0"/>
  <field name="valid_from" reuseLastValue="0"/>
  <field name="valid_to" reuseLastValue="0"/>
 </reuseLastValue>
 <dataDefinedFieldProperties/>
 <widgets/>
 <previewExpression>COALESCE( "site_id", '''' ) || coalesce('': '' || parcel_alias, '''') </previewExpression>
 <mapTip enabled="1"></mapTip>
 <layerGeometryType>2</layerGeometryType>
</qgis>
', '<StyledLayerDescriptor xmlns="http://www.opengis.net/sld" xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.1.0/StyledLayerDescriptor.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.1.0" xmlns:ogc="http://www.opengis.net/ogc" xmlns:se="http://www.opengis.net/se" xmlns:xlink="http://www.w3.org/1999/xlink">
 <NamedLayer>
  <se:Name>parcels</se:Name>
  <UserStyle>
   <se:Name>parcels</se:Name>
   <se:FeatureTypeStyle>
    <se:Rule>
     <se:Name>Single symbol</se:Name>
     <se:PolygonSymbolizer>
      <se:Fill>
       <se:SvgParameter name="fill">#3148e1</se:SvgParameter>
       <se:SvgParameter name="fill-opacity">0.1</se:SvgParameter>
      </se:Fill>
      <se:Stroke>
       <se:SvgParameter name="stroke">#2323ff</se:SvgParameter>
       <se:SvgParameter name="stroke-width">1</se:SvgParameter>
       <se:SvgParameter name="stroke-linejoin">bevel</se:SvgParameter>
      </se:Stroke>
     </se:PolygonSymbolizer>
    </se:Rule>
    <se:Rule>
     <se:MinScaleDenominator>0</se:MinScaleDenominator>
     <se:MaxScaleDenominator>50000</se:MaxScaleDenominator>
     <se:TextSymbolizer>
      <se:Label>
       <ogc:PropertyName>parcel_alias</ogc:PropertyName>
      </se:Label>
      <se:Font>
       <se:SvgParameter name="font-family">Open Sans</se:SvgParameter>
       <se:SvgParameter name="font-size">13</se:SvgParameter>
      </se:Font>
      <se:LabelPlacement>
       <se:PointPlacement>
        <se:AnchorPoint>
         <se:AnchorPointX>0</se:AnchorPointX>
         <se:AnchorPointY>0.5</se:AnchorPointY>
        </se:AnchorPoint>
       </se:PointPlacement>
      </se:LabelPlacement>
      <se:Fill>
       <se:SvgParameter name="fill">#323232</se:SvgParameter>
      </se:Fill>
      <se:VendorOption name="maxDisplacement">1</se:VendorOption>
     </se:TextSymbolizer>
    </se:Rule>
   </se:FeatureTypeStyle>
  </UserStyle>
 </NamedLayer>
</StyledLayerDescriptor>
', true, 'Fri Nov 22 16:07:11 2024', 'dominic.hinchley', NULL, '2024-09-20 19:14:09.231751', 'Polygon');


--
-- TOC entry 6239 (class 0 OID 39418)
-- Dependencies: 250
-- Data for Name: qgis_projects; Type: TABLE DATA; Schema: qgis; Owner: -
--

INSERT INTO qgis.qgis_projects VALUES ('Land Terrier', '{"last_modified_time": "2025-01-22 12:22:19.933806", "last_modified_user": "dominic.hinchley"}', '\x504b0304140002000800c962365ae720d4a883f10000d5000a002a0000007b33643565363061332d633339622d346431372d616464372d6133636366633235346362377d2e716773ecfd7977db3896300eff9f4fc1519f67929a122dee4bb5edf3736c2795a7e2a5e264aae7e4d4e18148d0e244225524e525ddfd819efffa33f4fbc55e2c5c408a92494a5e0b35d3b108e25e80c0c5bd17c05d76ffe3e8ecf0f3ff9c1f0b7f5c068970fee5edc70f87c2eb499ace7f1a8d66b1ebeeb8d16c845fee78a9f75a787df13f179f8f4f5eefbfda251057304e8228dc1ba83baab66388e771f03d86e14048c015fc92c0786fe045b3200cdc9d4910ba9329bca5ef8e400a3f0733b8375024451725595494cfb2f293a2fc24db25f8bbc574ba3738a228849f0b14f338fa5fe8a621c01806fbaf04617712cde0394827c21cfd830a47a4340dd229dcdf1dd1bfa424066102dc14755b98451e823f0a12309e422f03c970bf9b82cb4448605ae0ca5e1cc6097e4405096a2a00d318fac96d2284e8e10abe8be2194030bf7d4b07b41aaa78fd2ddd3fff74f67f0f3f5d7c1d9c5dbc7fab1ac248781b0769904c8453803b03a6c2fb38f006c3b70717c7ef8fcfde339507c3a383cf5f4ed063ec852074a170b188afe0ad10f908088294e0024128c836ae7dfcf1e387f38bb30f475f0707417c2bc8962a0d86866a9aba8166ca36868a6defa88a661b9a31fc787cfafef3cf5f4e3f7cfe3a98c1348683a1fcfbefbf0fcf3f7d3839468da21660781db893c1501a1e9cbeff784ceb7af012bd41853b926c6abaaad88a2edbb6a6aa0818b77d7c7ef17e30d414d344058767a7ff7dfce9e2c3d9e9d7c18a2f3f39fefcf31982fb8ca7089316144e60ec82348a070c42db9230c2f3834f0708e2f8d3d7c14784275d78108f079a86458c504671701984a879bb659f99162c4b926b2d44e1e5ca2644a557134ab5890b174ca1e023d28c62014d69bd1569c7b66d439215533687178707797b8b30486ff19455b1eb55ecefc0140d2704491a8497685424fc5ff3d457f1184d78c2284e27049128b7c66462a23abcf87a08e21426010887caefc383bf7d4064fee6f887c110776e78f6e9083583409ba932af7f8aea934e6400ca4a802f1707ef8fbf5e1c9e9d1fa3bea0590c218c51d785842ca1a19046f3e83206f309e23033309fa3773b03d4d0a7e383af832f6870a127fc820a112313de7cf9e507414434e027932886085618478bd00331598b5f7e39bc10ae033c348266fffb5f9afefa14d731e47fffeb5400a127a0c2df7089f2ef7f1dff5588428aa6ba86dfa05e4e51e5a1f01ba28884c05db8518acb7ed8113e245342852720cc11e0febe7d7bf6b7af9abd63ea43d146a4373464fcafb24348b99c08b41a25e9f7df77479829e50c0ab3366dff47fc672f9da13527fc3805a923ed6936fa1585e897a8083f7edb638950f8f106d720f32ffc788bebc8d9039c4ee7c91ec0bce7474c9fc9de4cf8318c1c0fb1cadd116d2d6f3b4122c4db5734d5dc1dd1dfe51bfc02f717bf615e80051a626f1f7fd04fd9fbac28afe1c1c48d8339662ffbeb19eeee88adcb0e08244202b87114decef6c9b0d0be575fe420e89b837912055efe82740f754ede1d2dbdcb812e614e7b3e9237fb3e5e5dbba35a299536a38ab8211269541549a80bf08a7c9b984c8087685644c2d883318c05e84d452f48522c3d443c257b0369204c82e934af1a2422168b5790bc990697935404df83d9028b5355d607044782165678898bf05c0f04a41f8cd192f244b4e42691b784f5bb48391aaa3fa87402d5dcc970322da34a8dfd985206bf37d0f4a55ecf16e8ad17c4d9b44c311c95d853700b6311f5188a9771b4986723e92e92349aa1b19bc3380d6052ccc6192182513ee0cdf596b00a688816603abd15e18d3b5d24f987b813e87e8368487e4d7ffae9903e0c04aab3fc7700af934103a4e8a2afa3c348907fc48d211544803773b4fe2179557478d597d4bf65f5d754bf87fc14a6f012918d835ac40d23aa73274e127c47dd1665c4f0d114a15e24883126b80ee2c28e091417aabee48c4ddb7434a0410728b2e580b1e4abbe0a806c8c570d48d65a329f06a93386137015607291f2a1621b429a61b48831f120e5f02a403f5e63ae28a6308e0318bf16bec1dbbdd718c2c1c0af05cc36f6088b10d2db39dc3bc1b4721e4d6f2f91fa47fa8354d61962dfbfc05bc4eb5d244af75ecbaf85146b847bfff9c7224aff3a5f8ca7814b7fefd03f6c9f6889f0062ddad90f4435bd0ad09a4308f706f32849913690b0d3270f98595a3d81c514d28e0f4ec07cc0be2ddf67c33489aedf41ac33c0432492d0505d81e9825222c510a0c251a581d15985ebada592519d4c36a49f1486a8ab2501a950f33dcbf15ca8399a26690e001ac221a9a60274cf745da546405f42b72d0955da6a4743d198b079efde48a8d2a7ae3424711ac2347475eddc5ea70e59ef4178154dafe00c86a9334132268a6f9db16be948af901dcdb46c47f381e7d81ab09db1ea010fa9e6a6e55a1b5355de5817eababc7fc2aa76eb8910980bd07779843717b4b5621829c25f2f52acac5769ae35d94af74ab6957256bf585210d669021fb17a7fe0cdd0166fa5c4979f90c4d754c3d481e93a8637f61ccd535cc7d224c519034df15460d88a6c6e24ea3bc878bc8a5a2e148cc2017898970539fdf9b4564536146b56c1a372de394033344d1cd7b414cf953d47552d440d06848e3d3675c7f465083d28b9be27f7a586ac8d76f4402b3b8bc5b638eb0a82c93ac595be5e64e3a3fa9368ea39b9f6a39ae658f615a4f6b948ed9365d5b14d1f38d6d80492a6da36f4edbee4135d87304e26c1bca5c29757cf6868333229b0b5e62df2c3f11666649e2a7f99429054280540d9542dd7700c53931c4d510d34e750758039d63cdf576457d27a538aeb2ed034b8b72d2925afbe1d4ac9b13d494a2947e69e29a5a732c5a8ebf42b8ea21908c2e4a99f9e78d1cc2137513e8c1d3cb489e37baa6f43b4fb85aa0ed16645828e0524d319fbba2d6b96e64255ed4be1cbcdb523f50a8ce3461e6c4dee1e9d880aad2f77e329127dd3603d553e89fb1ac3105e836946462ad06da4957bce58967447930062940ad2c8545706ba625a922c599b9051a5b57654c4826c87882a9d78aa34541ba9a74c426437358f832b80a49aa1c863d380920381e13b9a256b888254db91816768aa04c6baaa6d42416c631d367b79f7b642402cc6a74a3fd5717acae493df81e6924cb791ac321407fd419b42656c6149263bc0b70c19dab267daee2604546dae1d095560b64343d56e3c552aaa0fd653a6a3faf69092936cb9aaa4ab1662451640e4a4e88ea521a66440d5828a66fa0adc889cca9d5f077aaa026d87a06a1d79aa14b5345e4f99a49636929996e48f35d7f75d47b1d03fdad8d71c5b764d47975dcd841a546cd3dc88a68a3d62179aaa006d89a6aa1d79b234551fafa74c5378a49d0918072948117b923477ac1992e341a43369635977c69eebe21d9c6203cd705d38de84942aadb5232416643b6454e9c45325a2da483d7985db0529bc8cd0bc258e3b9620d2bb6524de24d5d164df776ccb331c055197eebbb60134b0b1ce5db6d741edce80b6a97797fd78d2aa373b5ccfe4f4eb63147d5bcc9ffce9577659935ff95a6357914c4f777415688ea6222dcf9280ed98866119b6e75abaa26e76899437d5e92e29efde03dd29e5edddf7d5d2e8b14c35f293fce9d4912c55f3f0e902347cc4f3245d7280690047725d5b73654d3255a5ef8c575a6a79cc49eb6fe338bfd2fabdb1b7479b43c2184b9d7a4af88d23ab32f090fae3689e8c141fdd301ca00315cd8ea4dbae61a986256f309da4d15243a48d3ec65d4d39b94ffad666f5903d559528ef717100901116d424d5b64de078b2ee399a6d290e50c68a032dd3f53dd3f064dbd898b08aed6c27c2daea753143584ff9e278f5903d01fda803b99d5d9c381729c0e7629ea32a0ad23074cb019aac201a03a663dbc07220d46c38f63dd9b37adb20a08684bca192aedc38d923ce01aaa59bff0966f3bffac46b8cfc24237973fb9d3c2ce2e95ee68d970653b883c62bc416f8309d81f94e145f8efe8ff9f6fbff318ff0df9becef2dfabb330f2f0986ef3370b327dbd9ef20dc936a24753d4bfa2a882b28e96e3aaa0ecb4adaa953ce4a8bfc5554d38f3ade47d1e5143a3fdf8e91cae7c8960a751d118561a1cfd0e078ecd89e0dd0f6de859e6a18aeaa6c60434adb12685b9b5048821d365379e79220246e9b57e9687a1b27ff473dbafd3f8a7183fe6644829e6ee9d32d7dfa4e9f30219544a3484f8b686a43757f5443ab8b51ec61bf9990f88656be11b1c0d97e170bb8dd1101a9c06fc376b9096f67ded684a4fb1268c2d2d945a40949773f81262c3d769c2c9a9c86285150dfab2689b49b84d483504c608a3d2d13218da630a60e4f32dad32470ea8b792dc208d05a23ae9fe4210853ecf7ea52272eb65a4188884b51bf65f423c18047708ebdbc909e7742ca31ce202c71526fafc22230d7177683d00bd0925e802925ffa2cf35652e2bae7f49b5df7d77f5ccfa12a8a720f940b6ff79b7471bf5abadadea0374a88febcb0374ab8747d703f46a3b3e1edd3b8a96ffbaf5b13b5a5aeaa4388653e281990b99fc5988a10f63b44851cdec5cae9f316c8e077a199a6ee45dfa6f1e2449e406a473fd8d731dc6eedae9d21316305786ea1d6071fb68d7554a603f8053ef13f499d178878b0a2644d13263def0ba98e87c8eee9eb29e66a94b73d66fead7cc5daf8e39d54dacd38b00aa38964c6a97da68398f75b4cb5359abd16336bbad9ca549ece20fb366ea3aad9accedc569df760e52f3aa2830b59d9002cdf24ce4af1aa7a07ca20c721e4d6f67518c9ddb3fe52fa8b736dad2ba20bc0238e24118a5f4d57f0749808f4f30bbcedcee26106d1e0e49cd5c95212c7d7f06b1f2b43ba24ff40dbc41c45c063ab8412c7f5f54545b36d41d59d6354337d1bf32fa0f15ed8ec8fbbcf22dadac4886b4239bb2a99886a228a6294baa8aeadeb2756f90e0dab7d02b5bdf316ccb302d0d6144350d5d91315ef4bac48b1e508b16fa6fc7b42cd99074db440b0aa9eaaa8611e79577476cff77e36c54f62534aaf9ef5779c0032484e898c5a506d736440d0f52c383d4f020353c48cd130e52f3b8616ad605aa5913aaa64db09acdc3d5f40a58d333644da7a0358d616b706193b8daa5c16a1079e3b36e22e32a05b9409fc730c151e70ea33045e21151dd1c520d6254a8106cc0367c2a32cd740c7a1e2b2ce61e48e1510caed19a39c3673a48af881730d72768b5bb9db7b3302ef8581e29f5781c4a558ae2a0e7ce5ea525f18ee030b58827351f4fa20ad116992e67da1b6a94741a919757f6a9b97ab58f185620bf7a9e0504c987f0ec0a5f99c16bb2a1be2a55b7caf16cd14be6d6a7ec479ff15bb283a907fc583d84d549bb7b0cebf5d70d629f739ed5a3283dd6282e05b8581a4de92146733b07415b1ddfbca87223d99275b0711f1a96e7a6ece3d1a6a9cbaef81159466dccca58038fc16dbb1d443c9d51635dec5753dbfd8d5bbf63b427347e8ce3f9638c5fcfe3ccad0e60271ebadadd7bcb2a58a3c3f4634c516f47f14754241ac6b2ee35fc5843d9c757fa698d64cd7bf6b106b287c7f0d31ac72527d2c71ac95eceb34f6b2c973d281f6b3037f1207d6263bae441f85863ba910be5d31ad4ba2fdd630d691f0fc22728862a0e658f2a893abbd23dde39c06aa7ad2debb04bae4f8f3143bd8cc29e0ea9d79d891e6308fbb84e3da9115ce5bff34883b9890fd3131cd765f795c71cd77e1e3c0fc48dfb8c73cd85a3eda16ad723d5d523dbc359a5d501cdd260360de526d70775df86871fba3e9e1cad48f1eeb1cb0b72cb267c537814b9dfbad93acd36b474d2155bb6cc1dd9d255d594104f532dc3546cbdc9d049366dddb2765403e9ed16da2d499aada812aebb6ce884f0ea96bc6320f163cab62a1b926d59aadd68e764dab666ef28926118b6a419a6662a06da8b7133276ee6c4cd9cb899133773e2664edccc691333277c9824966a44e6ea5a71a44133117d8248374f2634eba694bba69e4f814b1476187f98814b9c4e135b78e39a67e1699406feed09ea47f39b63c6ab26496fa7f0b03886d91b1c4ca717d5b281300109e9d45bb4f9f288ba134c11c7c4b9366b7e6b32fc11dfbd301d3fa926ebccfc75caef2edcdabcfd8352a772a0294b9e32f61d1d293e8ee64a63c71acb96634b48045b8a3fb6217681f4720503a4807ae1eea3e92b1fe8db6ff0f63a8abd8f41522a5ac43d1555a67fb3895faa48bd9eb056c7f62ef32224c5aff2d5c7b51aaed570ad866b355cabe15acd93d46a5855868ac71398022c2c4ba77b0f6de7033f80f13e16aec543915f15c4b88cadb55454fa0b87970ba480ec636999fdccdf612500a7f7c67f8ab27ace6f3a93e3248d11eb432f8a9f450b41981dcce43a00641e7d08130445fe14c95e4337c2f9475179f1b3088111b3512fda0bf14c8cd7d64db1729668baa06a6989a473a25e22e892a49709b946ca2b88b5915cd7526a13adae23d18e44ba824c49d0064699a2274c8c8b6213e5926011f9d4efe2564a041f32cac4eae73e5a6395e7bcd227388baef21ae543fefa0282d89de4ef99a7bcc239b6a849c9ae20ff997598e9cc5612d89ecd018e5085fb5179a655509f420f6bdcb827e5c3ab6cf106617aecfb68be8b0d49c31684f5c1200579fb4806fea9f62831523b603ca8912243bf9957aa84b624ba64eda848a1b56c24ab6cc93055038b2cf6b8b6f44cad039886aa488a6198d533dbfcd4767d03e5c16d7e74bb1e3f73d03baa9d3d5f5f2696b6e22b654b5af13996be23e9b2ac58a6891470c390547dc587c8a68d7584f23f53d6ade66f588594ed7d4377f12eb257101c8f9522f9e6f1c985735adeda366f6e9bb7b78d1b5c2c59d2340ec60be26d3f41cb706f907fcaf5f575fe29201d216e340bc8d9464278fa02097d248291ccc51844f2bc334967d3c1fe09980bb8abc2bfff9f22c9ba40e76477c434558d2b42b6d2959ba7a53d767597dd4744173beddfde5f089686d4bdf3042ebc482c3793953d36ad8676cda717c7276f3fa26dc26f513cf584f710f18b14ed0a2e6e118798a1bd354206c304ce10fbc0bbd593b77847b4a6f21bb2930dd21f907edea6fa7b53955ad7b54cb5755d59d6db23960d536b5fd93494d6951559b57165e67c221f7cc4e62c5935878a6ded28baa928aa6ea8abf674f94c1d1c1e7ef97470f83f5fd12e678b0716aa62d40e2ccea3f9620a6201099c059822014256d11259e50718ebaa371e69c892a2b53fd290eefd4443dac68146edc8611ba70d9b9e33649d11defcaddb7943de03e1cdfff43b78f80d8ef3c305b29fbf6229a33c6ea0ab670cd36b44c102968ec6bfff754120b287d362af4f24b231c4227b487fa25f1542c6f26c69a75fdbebd3ad3ed8cbd69ff0e3b8fc890f00d0c65dca0f0024badfcfb6fa123e0d90d9dd3df02e11474ff6febf70319d0a3fa26691d85ebdeb2f7648b89f2b3749f9cbe67d5221b7efdc30ad12065d3652776efbd76cfc55e9617655ec11f5aa0380bb8f00ba1d02ac3f06c80f02885203d3ea79c0aa1381f567024ba7024be702cd2703ebcf06eaa7031d958f552704ebce08d69e12ac3927587f52d0f6aca0cf6941f7f382ceb4bd92ba6ba706f57383d52707f4434984d4fdeb196545f4297f1d4647d505420bb0062d8c11ff3d8d88e1cf228117b17b4aded56c8eaa18f0363a0f4c87f7b0e20c84684dc482bb88f122da4362d5078b69e59ea806931b74671599b656202f1bcfbefe4b3c658fcad81393966726779e9a54cf4da4a67393352727b5b313cc0de06c1e615dc487a93b296266b22102675961c554f2067a9f10e381d5e59b221d009127fd5b61d3a147388057b55fac63415c29eb4f793e30855754a1c37441a8e27b921d3954fbf91ddf13406a4a97dccec6d134babcdd1b7c449ba941957101114d322af6c46d0407af04e65d1bfdbb0e59b6bebe1e4659b4e046d32965179d238ddff9e978d962633e3c66176408abf34b8a842071401820a69c0d3c98ce27804c873bc56e4f9143f90429429b7b173af1840df48c4a63f4c389110a54a9883739ad4e55315d4ed667a779bada4cd966d3d665eab6337dcd53584ee39d43424f00d08c8024d91b5c04b3399d561ce91b97641145ffaeb886e16bb224da5003a2064c45b4554d173d776c40cbb000f0e47f5623ee4e236ae82a0d369f05a4955f868e0792898344500ae3b01829a9f3a4b8604e18748122f9638174a8ee78c85131ee548149ffabb2011a074910076bee0e615d0556f5e6276958fcdf260d60e405da9393cea8d819702823dd60221ab0ddd31034b5b4f158c4e0da0942a41e42674e931a6d3016ff1b215415aa1cc32b38ed8c07334707718f282e1029ba3e94756ba8a843f4b31fc6da8289a66be3c9af41741d7869b95ea41d6303341b4fe1c6147caf44bb253ac5b51c24aef17d4aff2f45d5668e87146f6ce9e420156d9bb8ee690097dbd9742cab188902bbb57120d81e6424684b1b8fc53504df9c2a670d11eb89b173d506a38276730e15594e45b676c74499c47646b459cb6aaf79b6d37a36d53ebbe99fdbd240d78c4e4b2d745471f1cb0e1b6a3b0b723eb06adb916f49de05d3e9236c4950d353be25b9bf2d099ed6fa960448ae66faba86b62496276abe6a88f6d81a8b63a8000948b22c4bfe3d6e49c624af89b34d1eb3a4b86f538ddb968259d799862f536b8a1669832aadea43faff7d662047599d8530ea8da8ae4b2bc666a8361eb43ebb042e589f8c605d169ef80e213b58dd72bab25f4922848bdbd01d816c308a05e1801607972bb1a529702724cc297a87e172c47fff673f8cee345a78ce767b893315c334c6911db048724adfa57654b912f17c122135664b5f5ea4a1c0b11266e026982d664ee6dcbd31727c77f80def4d8b7d490cff5804310d518bfa4f5aa36edb25032e044210a65d1b82711c85305a24cc5608f8f07281c304e49f576b3073a3a76d8ea368dab5d11904c922fba80a1fd8e44bd260561d2e92040d217612e846a1b7d9801150229ec78b34459b3a82dd0768bcd209b6d98ca65ee7cfc82ea86f47d48eaf80ff12664c6c0bf909e96ddde760ce6a7dfbe4660e1596b78ec11c8a77df7274e066edc5493b31b2a9f8581eb036df4c06a6baa7aadcc7e6e60b00a9e4d866e67b84989877b6483f156527309d4468d8430850c5f41406979371b488193d3c5b5168bde198113920d26f0719c20f610b7ca39afcaa7574971a10533751b49b286e05c3080fc221d6eaf04c45d4849bbc22bbc1b7a4a258a4fe4b5027a61083134d100357e98036446c0a8925867b5b9d49c4c84ed0a7121baefa462798e15814a7882921414d1f6a97fad4c2f637b4e2e027d250dd70b8bc534d0f5c778118c4edfe7192d24d2eb9632dcbab20ee62b69812db89c345fa31ba46c327ed480a5e5b4b2fd6407e99cf09a46dd520e98b7a37bd2378f58eb8d3ed2bb87bcc734549691eb6dd51755e993763547592864888220698e26ac22598cdb2ed7d5644d85559b31e2b67b24084878deec8e223131e7c871745222a5942e09731b8253b96e21a3c081121a784a6e8fe3487fc84e99d6c15f212620589d3f65903a16caa0274567d7c4b563d06183590375927d42da0be9a2ad58b0575916223a0f2f91d391826261a951a15c6513cadf57118e54e0adc6781fb2c3c1f9f853ed97fb7e4b2b0cd1ce58fe19fb0dc6f379adf12f63ad8fff7ff13ced0fb0bf21e3b2510268c91213639140e0fc5b7ff235e1cdce598c00613e37e09dc2f81fb2570bf04ee97c0fd12b85fc2a3f82554453ad65e92a7e1aad0d8b195de0bd5da41228c176823248c6f0580f494d98cc46fc1cc0eaf43b4af12d209484b0d069255865db8497010e26e09c6d12215e2087848bb41ad0453f4d705feffefffa1bff8f11adc0a4916c56848112cdc89308b62881ed12288d00e0e3504856bbcb2771a3d2c70172a25d82a1eeb43bb23562d22e548210361f01d64fe0695c70aed4648eda0758a9fecfbab2820b19de85ff68d8ff4ebdd91cfeae398aad1c04cb12137f95b392c89885309f9c3ba0fd43f8a3a9254c3e8a21221411b62bcf9c30af41d9a28b9c0277adf4060162d7e9ca17de767b265cbeff52f08e5e71bb9df7efbeda78f1f4e7fa91e32d5bad4d6b58568c3c93ede66626222f34ee8c58fa3594d43c6efaa25ef48201ba211bcc935e61f7604a45baffd7a8289d5b71131651d61c7d8c50a2a590b027693100e11e523e2148a9231eef5475a4f78737634fef8031e080ab78ce91087cc411a7781e8a054edc58b095afd07d3e01b643ea509dbbd3b03719d9febfc5ce7e73a3fd7f99b75fea7a4f5dfa5f7afd7fc5beafe9b6aff7df5ff0d7600f7efbfd92465f1a9f8cddea07a248d4f662d0b7170c59254a46c21ed2a24914ac89dde0cdc20887500a8c6f7bd4179ca3b10f049eaa0983172167f7b47aba84a0d09427bbbb6e1ea3d5ddda5309f57180791b7647eb3d28771951f637699b2846dd997b1e1a09d3bd072075aee40cb1d68b903edb375a0d52d24c974df177d5985a2e6eb401c4b8a246a634b312d178c4dd7e60eb4dc81963bd03e3d075ad99686b2690d655de61eb4dc83967bd0720f5aee41cb3d68b9072df7a07dd61eb486aa9b96a60211d8b68df624d0102dc35345a01bb23d7681ea4be02579d06eaac771175aee42cb5d68b964e52eb4dc8596bbd0b676a1255e53bd5be4feb314ff6f27176f51ef7102edd0fb8859523f1f6584e77c319e06c9045fec51c3b72ff1b41f32381b43cf83de6f817709d364e4e22ce5dc3598bb0673d760ee1afc0c5c8375ee1afcb8aec1b912b737388d8a878170fd6d4c0db44fa3f745e1b374238e21f0cec2e96d4f9fe22b88e97fc0faab7ad1ccc9f39b128d28717cdd86aa6a280efae3399a32b61c0b48b2037ccb90a12d7ba6edaef25a4d90de13b870ef35ce642a62ca0960fc5af8066ff75e579a41fb780fbe16dc0974bf9dc7888bc4b7bfc0db2f6140b8e46bf9b590e2aeef0d5007411026839dc1725707f7e3a65aba852e37790f4ea33cdf24cf8cc233a3f0cc280f901945c8671d09abfd7994a497316cb2f4a482829c508faa6cf108ceb14a14ba01ac26ec05cd6fca73141fef6899378f6afc09163748b222ae4ef6bea3c7370a95b767148a66819c1e608504fd3e2e668038af659a605103b89932bd6c434a760a85f2496ea90bb059a5b8b90d9c597d6f80d4948140ac468f182df4b1ed52d1da8917b8c3ac8ec6d8a88e8390ae934334b57184cfcd0b904c154be6689510faa167b4d9c525d26e09e067a4fc00fcedc924ba3e01f1b7bcde87f06241ce6fcea7519adc69059be96e1f507fd0225d80e93b88f579a4fd70fb586e1fcbefa29bec636d43d74d55b744c58250d4d06e461c9bb227425b55e1d8b265d985dc3e96dbc772fbd827681fabc9436c5aa1f30433dc3c969bc772f3586e1ecbcd63b9792c378f7ddee6b1635906aea2f8e2184886a841d3166d4df744e099e3b16c2812b09517651ebb991ac7ad63b7601d2b4bf2d0d086b2646fd13eb69f36bdd244965bc872e1ba4d0bd952b8b267ae0f2a5e67a4612e60ef4fc0d2a9ad8b584f1d9baa04c722303d57d46459176d5d554475ec43a8f916f4c7f729620136e0daeca0cfd9ce49df3605f1041b07e1888a53b45a5cf4e4cca38031cb941f4db457e8de0bc02c0a3dae1f70fda0a21fdcd3ac6c5707c15dc376ee93c8ab10342487605db105df99efec057e4fa346506f3a58d8023270fb7323aebc3d19e5ad49417b0c0727c463a7cc4dfb93746fdaa88fdcb989e7077cd9fe4d1d1c77f2c5400192c24efc3c866ec098f7cca22b78b4984f91b049e169e4c1a466b78457e8153cc416cc49d62b4a92d898b89111ddbdd676472c56d63613171c622bbacbcc8a8ab176ab7d5269b1474cd8b3d6322b393133022766491fc1184e3f925ad5488d31f421f63181d4398cb585c31357e948c5940bbdc44e10e55b6cbb9e60d37bfc63509c46d64dc1abe305bd20a55e6059e7eb9b69dac48a3d5bddec68b45c1b8999a289aa1519fa804d3fe8997f0b632ffbdcbf249a39f4dc1bed489ee0a7648f8dab69174c039054941d5222f8d404b361090941e8c11bf6806ad40aba0094ef0664892307535a80b13391c3a9cb7088ff553f7b37411c383d477b4ab766d4878b6ed78d06adb137289878a57b6be0bb815646a41360654cd6406283f08641d8cd387a258d042d5a372cb062ab0be6f3e9ed59f865ee6516c0a396d836415419b20dd05406b02d1eec10501d37bcd8b18b05d229926a7294ac74dd70a2fdec1f0b7c0652f8ee61dfcf144724af16328d10d247fd652a543ff48ea61b5a95dab42a776db532510fd5666556b7df28e1d6f5e966fa51dab27727063c5ec4ecfe665ee38677ce6817d0cab47402ac8ced0a4876801a06638d4b479e7b11d2cd6b03633a202f883fe0e0108457a0dc3bfc5dcafe131bfec9fffb674554ac68aeec07f121a4c25948a2ba9f022e39c3a341af7f08122ab1896326b6998ce647d135ab06a1419d2e66ac5bcca8a93596ae3c920309edec30d24a329fe89a968deaba4dad98cc48339a5db4cb89a157ce09eb7b8340701004218da63006f4266b9f6a26b87ca922da05a7a3c6524cde999ba55422a8bd6902c4e73c73904ef6ab5045f1aac6f677ffe3ebe1d1c1e783afbfffbebfdc62b91d8180bc4816733202b87b4b656c23513c05b7d122dd479b1e886fee3cfa4cdb28deb220785a593728b4673a0b3f477376bbb448e04790a4ff8d69b9ee1745ceccc886f7bcd88e9675aeb368178cb316bc0ae07549abc42bad5ed63a0e057797deb2bb3482209fe8a4305cc430f39a962d579574d57234cb028ea628ba6369b2e61850b5a0a299be027b794d23f683f6dff8486c03b7e92a9287f19baeb5c91da7b9e334779ce68ed3dc719a3b4e73c769ee38cd1da7b9e334779c7e6a8ed3ead89375c3d2440348d884d235446b6cd9e25895a0aac86309e23318ee38cd1da7b9e3f453739c56747d28ebd650e18ed3dc719a3b4e73c769ee38cd1da7b9e334779c7ede8ed39eae7b8aefaaa26ee9684b626a6837625abaa8b8c09335d773a121bf24c7e90dd538ee38bd0dc7284b19cab23a940dee38cd1da7b9e334779c7ed18ed3635982baa179a2ec7a86a8a9161081e2ea2250355df325495280fbe7739cde501073c769ae1f70c769ee38cd1da7b9f2c61da7b9e334779ce68ed3dc719a3b4e6fe438dd600dfecc7d74ab5fc45da7b9ebf4bdbb4e372da20ebed3358a7dfeced38de3d1c593b93e222fc57dba7160fafb2bd787e9cfe740dd38a00fe4415d1ffd769ec552ad59e9015ca8376ff399b850379243177fe6fa9c72276aee44cd9da8b9133577a27eea4ed453889a5bf6a256fdb1e6fabeeb2816fa471bfb9a63cbaee9e8b2ab9950838a6d9abdbca85d773107a1bb51f2e92a9207f2a2aeb6c9bda8b91735f7a2e65ed4dc8b9a7b51732f6aee45cdbda8b91735f7a27e6a5ed4636d6cfa9a628b926d6ba226ab63d11e0345448202e832f06dcf37b81735f7a2e65ed44fd08b5a958969a7a9702f6aee45cdbda8b91735f7a2e65ed4dc8b9a7b513f6b2f6a5db56d53f35d716c8e81a8e9ba2eda86e28bba66daaa677ab66f4a2fca8b7a33358e7b516fc34bcad087963cd465ee44cd9da8b9133577a27ed14ed4b63cd67d6b0c45d9b39084353d288e151f8a50f15ce879baaf43f82774a2de4c0e73276aae1e701f6aee43cd7da8b9eec67da8b90f35f7a1e63ed4dc879afb506fe443bd6c0bfedc7da82b5fc47da8b90ff5fdfb50372ca22e3ed4558a7d013ed44de3d1c987ba36222fc687ba696036f0a1ae0dd39fd087ba69401fca87ba36fadc87faf17da89bc8a1930f756d4eb90f35f7a1e63ed4dc879afb503f751f6aec9bec4cc01811469a38b2a4b963cd901c0f42c9d1c6b2ee8c3dd775345f526ca019ae0bc77d5ca7d9567a3a4e573afa206ed39516b9d334779ae64ed3dc699a3b4d73a769ee34cd9da6b9d334779ae64ed34fcd695a87922d8fc7be288fa1266a1654454b8292a86ac090155d775d97a79ee64ed3dc69fa093a4dcbb6345424736849dc699a3b4d73a769ee34cd9da6b9d334779ae64ed3cfda695a0263cf530c43f4c6b2296a8ae28b365455d1d32d15ea1284be6abf24a7e90dd538ee34bd0daf28d518ca9a35d44dee35cdbda6b9d734f79a7ed15ed3a62ca9b20a155151812d6a32fa676c188aa82ad0d30c59730dcffaf3794d6f2888b9d734d70fb8db34779be66ed35c79e36ed3dc6d9abb4d73b769ee36cddda637729b5eb2037fe6deb9ecf7709769ee327def2ed3cb0ba883c374855a9fbfbb74c35874f15bae8ec64b71956e1894fe7ec9d521faf3b949370ce603394957479ebb483fba8b74032974f155aece27778fe6eed1dc3d9abb4773f7e8a7ee1e8d240abc06d33cb334d06dcd533c672c4bbaa349c070c68aa93baa2b035d312d4996ac3eeed16c2b3ddda32b1d7d10f7e84a8bdc3d9abb473fa87bf411ea7e304d84eb0948116f98cf619820d927a4132814d16804e0a38546caf07918de1a084142ac07a0b7c33dacb98735f7b0e61ed6dcc39a7b58730f6bee61cdad2d5b78581b96a6a838fd9ba2a9aea8b9922e02491d8b860d357face806f46cee61cd3dacb987f5134c4bade843cb1ecaaac93dacb98735f7b0e61ed6dcc39a7b58730f6bee61fdac3dac3dcd53145bd145d79380a8a996290245b744dd565ca88c55a003e345a5a5de4c8de31ed65bc93b290f0d6d685bdcc19a3b5873076bee60fda21dac155fd121547cd1551545d46cdd1601d414d104960f54c9963445f913a6a5de4c0e73076bae1e70ff6aee5fcdfdabb9eec6fdabb97f35f7afe6fed5dcbf9afb576fe25fbd6448fecc5d79d9efe1fed5dcbffadefdab97175007ffea0ab53e7fffea86b1e8e2ec5c1d8d97e25fdd3028fd1d9aab43f4e7f3af6e18cc07f2afae8e3cf7af7e74ffea0652e8e2ec5c9d4fee5fcdfdabb97f35f7afe6fed54fddbf3a0952c4ee8b6e38ee58828622cb8ea648aaa3c9beefd89667388a2c69baefda06d0401f0f6bb69ddb9e2ed6b5be3e889375ad4dee66cdddac79166aee23cd7da4b98f34f791e63ed2dc479afb48731fe9a7e6232da963d703aa2dda8aa28b1a048a68435d12e1782c015b02ba7cbfe692dc479afb48731fe9fe59a865d31acabacc9da4b993347792e64ed2dc499a3b49732769ee24fdac9da4e531d481e429a2ee024bd45c4d1681e649a2add9aeaa9892e42afe4b4b43bd891ec7bda4b79567523187b2c4fda4b99f34f793e67ed22fdb4f5a8248a44aae295a968a84aca1d92230155f34fdb167404f72b5f1f8cf99887a1351cc1da5b986c05da5b9ab347795e6ea1b7795e6aed2dc559abb4a3f4157e9a52bcf0598623bf3657bf324c7b0e4d0bce4d2fc9f7f2ca2f4af8c0d212de86ca6c2bdb89f9117f7b2b1fa33f718ae7c10f7e3e67edcf7eec7ddb0843a387257e9f5f97b72378d4617bfeada78bc145feea661e9ef3d5d1ba43f9f3777d3703e903b776decb93ff7a3fb7337114317efeada8c728fee07f6e8a6fb8ce559ccf71f9bf87b5734015a264c02cf8321239d9b08889c75ee0d44b9b42441435cdbfdb44258e05215ab0fb20a8d65a84ca957bfaa54d7e713e506b06cd2abbba7daf873777bee6e7fdfeef69595c27def9f9ceffd1cbbf5b9b78ea1c863d380920381e13b9a256bce58516d47069ea1a91218ebaad6dbf13e6f6403bffb0cc5c339dd670d728f7bee71cf3deeb9c73df7b8e71ef7dce39e7bdc738f7bee71cf3dee9f9ac7bdebb9ea58f66dd15480296abaec8a63801ec76377acc892a9599acf3deeb9c73df7b87f821ef7aa3a942d6528cbdce39e7bdc738f7bee71cf3deeb9c73df7b8e71ef7cfdbe35ed7c6ae35866351871e1435a068a23df62551974ccf9081e6e99afca23cee37d4e3b8c7fd16fce96c7d28abd2d0b4b9bf3df7b7e7fef6dcdffe45fbdb8f2d55b6c70088ea589744cd9654710c5c28ba926b68c0068a0ecd3fa1bffd868298fbdb73fd807bdb736f7bee6dcf9537ee6dcfbdedb9b73df7b6e789c9b94bfbc62eedac19f84bf068cf6dd2b9433b77687f1087f6ca02eaeacf9e53eb0b7167af8e45676ff662345e94337b755036f4652f86e84feaca5e1dcc87f4642f467efbfedddb7264dfbccde7e4c85e2585ce7eecc57c7237769e989c7b4a734f699e98fca93b4723ce1826684b4c0e5512c7f754df865073a0aa4347f325e85840329db1afdbb266692e54d53eeed195667afa4757bbfa201ed2d526b98f34f7917e501fe923d4fd609a08d7139022fe309fc33041f24f482750885c77310721da7b011fad355286cfc4f0f6400812624100bd1dee66cdddacb99b3577b3e66ed6dccd9abb5973376b6e71d9c2cd5a56241328862b2aaaea8b9a36b645c4d635716c4aaa298f3d1d4863ee66cdddacb99bf51374b3d6f4a1a50cd1bfdcc99a3b5973276bee64cd9dacb9933577b2e64ed6cfd9c97a6cc89aa62892e85bba2a6aaa218b96eb9ba20425d5523c4db3ccf18b72b2de488be32ed6db48592a6943dd1eaa0a77b1e62ed6dcc59abb58bf68176b5bf614179a92a85aae85a39818a20d644f543557b225d9344dc5ff13ba586f2486b98335d70eb8833577b0e60ed65c75e30ed6dcc19a3b5873076bee60cd1dac3771b05e36247fe6cebc950fe22ed6dcc5fade5dac1b9650071feb2abd3e7f27eba6d1e8e2f25c1b8f97e266dd342cfd1d9b6b83f4e773b46e1ace07f2b4ae8d3dcf19fee8aed64dc4d0c5f1b936a3dcd9fa59395bb74a96dd44221b6505afd1cc46b89a92828b3c2738f774e79eee3c27f8d3777b47b5c9e739290c173174a06a9a63d9571ccf7535479365d5b14d1f38d6d80492a6da36f4ed3e3eef88fdc3981c492e1681d7c6df1d237180875dde760605f8bd7bba172d710777eee0fef849c05d6c32e056898690e4ee88a54c521ec597200cbe836c8e2b8f95498e9220ab53fc64df5f456815e37543feb26f7c70831de1d1bf15129881608ae79dfc65dfc411f964f2a7a27ed43e8a3be17327fcc209bf69cab0b0c3276ba66d1ab62aab9a6528aaac9b92256bba626a8a6aaab28904946e98b6259912faab2bbaa569b66de8b665cba6661a9684611443322d49522c5db54c43d22d5bd72d43554cc3b04c4b36655d93345caaeab2a65a8a6620f49aaac8aa6258968d1e650b55b634d404fa9fa4eaaaa9cb866d19126a4d9674d3d07559b114c934144dd76c49b225d5b25505016b92891ab374c9d2502555b5116edc9aaaa806aaa5288aad59b2a15b92a5ebb68a5a5554f4b5e813d1a75b8aa1489aac6926fa78d45915f5d6942dc9966dc5b62d4b567409f55d520cd9d26445b3105e0329061ed26942a258935346a4fadce02d021fcc3e838946ef3b3d9f8913e2bd1f84b79c32fb0e6610d2c144a37acb69b2e730560f05ea911a72d900e320f296ae6256868658151e22b33d59c2b61c22825cdf5458fafd845561dac3cfc20ccea2f8f610a03d065da8e40efb1c499f00f1bd69147d5bcc1db4270231dab4923ded293547abec5110dbbcc526192ed610081e8c9cdce37e405bbaab6b8784792c3650195ea84968976402c79375cfd16c4b718032561c6899aeef998627db06ea12485ce041ef084e61162d04edfd0e2b1dc5a7afd9cebd687e4d6f1773b4ff4acfc2632f48eb76b8bb056c72b11827b0aed1d09b4026ca3531b41aada9857464174e1d72cdb4b662b6afc4fa1c711e6d51179162ba6885758e060dd6c35aacfe56faae24c6061ae291795e60649ef90237994007cca245983a97e379cf603de5a223049df901d3003e320fe0c303f8f0003e3c80cf130be0e38f7538568127babeab8a9a26bba205745774a1afdabe028166aa3c800f0fe0c303f83cbd003e681b38946575682a3c820f8fe0c323f8f0083e3c820f8fe0c323f8f0083ecf3a828f6fa8a60d24204a9ea1899a3576450055286a606cfbb661289269bda4083e1baa713c84cf369cf40d7d68c9435de6217c78081f1ec28787f079d1217cc6c6d87615c915c700a8a2a64055b464608b9a8d64aca629ee5895ff7c217c3694c33c860f570f780c1f1ec387c7f0e1ba1b8fe1c363f83c720c9ffffc6311a57f9d4290545c7800944dd5720dc73035c9d114d570c610aa0e30c79ae7fb8aec4a5a61b6862f18b0fd98d3c71168090863a2bdfa49d086c2363ac8a27e729dacdae0f5eb5e1547d1b1171da049dea0bd3f6b6ca6a53bec05986217ca6557ca24c7b01441692986d2e1c1c5f17ffee5c6fb2bfa07fcf5b79f8f4f05ec77476852d8135e7f8e91c0792d7cc62f0ecf0e3e1e5f1c1ebf6932821b0aaf4f3e5c5c7c387d2f1c1d7c3e7efd83f08f7f08af857df43ff4a3c059b675fcf1e2f84e94a71149e159c3e64648134b5cf8a6408baa7e09bf85e837aa7a7c7a846b15b885d73f5138bab6ca458a1db16819827f2d541b29c033bbde9a1deb50a896539bd51f3a1a6cd5e714cec6d0f3a0f75be64feb6273c5ce9471056262a489ed849312066109bc3670c401781de06387091b3d9d885eac2b7c26b485bc0e8e61862bed0dfa088681506b2b37ba4e1cd7b414cf953d47552dcfd10c081d7b6cea8ee9cb107a50727d8f18a3e6613b0e9224720390451bf2faf5c7c94cbe89a4ebd21316702010adf894b1214ff223c47aa7d8f6fc6ff07620e44e00bfe0b817b96f408612dbe4771b1facc65f106704b41f5be3baccf4e3b5805df7f614d394244a3d27887282736a4274b75373a66b149ecdf47987fec97a4f1f843798767fa82af1c4b8fc13f419cac82d9cd92166e8afe175651de734cb3a683413f87622d6d59c16d60713fb0c6f52eccdd029a8588743ca0f09993dba9968173ab209cd9704fe9ccea65d50ac3a04beff586e2c25f0d17fe8d1afea1e7c021e7c020a8d918ffd838f7dc9fbdb862b258ae8a74c44ddd7541c4ca7d135998d4d668260395d4ccbb948e345471c476c28a90c0bd506188f7f5ad0f9acf81d31223dee78dad584e917ac89e547710d53da151f5133cab3fe68b6748c8211268e6cb9aaa4ab96a35916703445d11d4b9335c780aa0515cdf415e8f66bfcb4727986a370553e2be987f53c53562bd84ba5b50f4aaaad96d74bade2ed1433f3baad7e8a8600046152514e1b86a527259e46fe210d3fd6e238a809018935f7f69630870d99e761846fd059ebea1e78aa1da90e526f73808764c78c2be37a7e7c842ae2f858f7c58a0166a24eb81917c5d774a107f075d87c31df0091172438ae98e39308312583f0462727a35bf45f67d2275353c78731892727a2e76d84cf89ae607c1d07e9466b82620c92a8decba7bda969f0ef5d4fc9c437f65e358a0d7581137053b2471ade2d3bad8a16f812e86e0441834b535be0e274aec5d96313fc450ae7cbdcbd6deb1715238a8b7910be8d6e3a2f8e8b85ef07e520be7f7bfe2c987118a5156fe6a7b23be94ec2cf747382afe35bcd02d2a0a8e87fe2e1fb172478b7e780946f7a1f6df0c7b77cf01f379d083f707801070e4da1c0373e6fa8204d1cdf537d1b42cd81aa0e1dcd97a06301c974c6be6ecb9aa5b95055b774d0506df9399e332c4fc8c6c70cd541e1a70c4da70cb508f64f56afcd8c24ca98625c083ed2145402b6f15978a459a847c3e3e76dfcbcedf99db73599bedd074f193df6a7d1f8924ff8cb36497d570f20da3eed5dc5daa87dd2bb9af54387bc77e5b5fd9aa477abbf6f39b79fd6059c0df598c1eb2d06a929e864066e74699e9e45e5a0661750e62c2b87b7ee86670e8c7228bb3dd4f8b6a489ae2914ab9324b7a0a92505b300565a035754a3025e6d0d5f17ea050aad2b8a8c951608f4ae0828c32ae08deda586ac738b22d122b59ea5db93d5591a2b2ca3236c9d6f740467984747c8460ed21b07cb46ba8e5e132fe9dd918c21f48667b94a47242c6be9098af94b47d0b5694d5b12d1129fe987a0ca6bfae158e2371ba1c9794e979cab8d4ce75e52afd6d94eff34a7150ed41f4d9d19f5c7c4f0a5fe481a59d436d0b1dc6a83416f625cdbe85ec67eb6818a6567fdf1b19c6d732c98c96d29c3f0c634b1ccfa36c655e5821ba35b6288dbc298f3c66de1cb38e503e4585eca0ef130f9952b4c76fb49871bdbac73e4074ab0ccb0ef07fad0465effe06db382e18186ba518a3c50db4b22e7c1db65e5d30335ce0ab307223056f23d5093adf2d0df13f35892a90fdb7055003f6cdb4bd2fa519acf45fba3349ee901db6f9b4d3adfa4343424a26fa53eacc8717fb70ed005b02ec8bbc032d2b80b58a348ed8780958b9d86ab49b8f5eb422624fa01b362a60b065656f481c30cbf0b5c13d7ee44294bacb70774957ff640b0c404fbe3c839597f0c193b5a8180e5290dfc634df23390a671305ea49046706a38e639202f8829d1e0108457a034d7f9bb94fd2736fc93fff7cfcac1f68ae6ca7e10f3237ad5262451355557e99b4f2395647647b81ab105a2715109667a2947ccd4712e91687e844006950127a64795eb3c5a264c02cf832173cd5667b524901ebe3b612294a361ad05ee58856d99ba37c35761e6192ac55036e85a75e9642845b90fc6bac0d80819234136c2d32852369b846621b3d9d7ae60435bfaf44c8e64d8544bd90c1d2b9b36ea61a3b0dd08232bc6369b6556906ed4a52609b94562c9e5cd16516602a80f46b9012ce3ffd55027359ecc880b462ab02ab31790a4e95312d3b512e827baae9791148d3857e718b8df2e63445b1e8d2fbb37f88b0d3ccb2caeb2ffe7b7cfc2bb8c6f0d96aa3b59e06d1283365e4c59bec4c4e342458df1a770958fc1157c5d3746f12324fb910c0fbec17482dabb9c64a92d8908c33b1124e1b0411e2966acbff7062717c2c5044ea7c2d1f4525086d68e3a14e5a13ed42bf14f8314317497408fa3a957db10911b20dc56bb117365fc7f6836e14dda302e6531aaab1bf8fff2d1fd3948d2280e5ce12ce71dedc678c5581ea361f19ef9605a1e90fdf1a0b08b0e459cb9332531d93a12e07f4ed3bfee174478707af4dc897077d4b096a9ad182dad2ac1cd1c6117931cf44a35984d0c8c40b0659f90465388d83209a0bf4f4dbb70f952c5200cd2516329e6e33477364e93bbe24d1320169e73904ef6ab5045f1aac6f677ffe3ebe1d1c1e783af7f11c4ff12059a96fb276191faa2854b5e0d068357bfbeff70813302cc12c105882d832b2800e1fc369d20bdde5f8454c14f272015025c653a859e703d81b80c124054fe2a9ac3107a3baf5e7d49207a816a96a091003c8f249c0588de2ed1e24645b7e87b69b308e818875324f830910b914f71e71802dad620ebd407f47dc5cbc12b32db3baf0e701a5c800df5115a6c179bfc44becf8fa399f0c76590ec9cdffe9aeefc9a59e4254280d3e9a6c2aff4f9d52bb4a31166d47ad5c19ff3c60b00eaee9086271b0a3ecd54fbc34f64bc71742fb450b2c29d3c50dd9b1fc85b446b691c4d51058a64c70f42ef70124cbd37597b436170728bb32911d3c31f5efdfefbfe325194b1bb503364b0167342a4988296ca583a8862d4eb6891ee63d9487e51ec45f9d2d6ea98bc7dc7d0344957308653b24f12c8cf43ca93e882252c065bd1c681073fb2af6bc5efa27039a3f9347fb31d8e62ee58dd390a8ef19c7f206b9f551d93439c3120c0fc13c79f06f30432d9aad98ccb24c06f300dd2db6a0ae9222021d176ca240417690c53373f21cc50534c596470b6461376b6752a1fdea3751883294188d4a543ac2513134d2c24e66fa31bc615fc33180f9abe09f57979aaee850ab64f07bd254b132534ac0f6286dc66429ba63033eb13e556ee35f73cf24f66ecd78cfea869f81f7e7e56fadef0297aa42952d70784e3f3f248f3a2b48894f8ec26a7976ef10427476f1d3a8aaf9fa7227ad6391cf2597aa459d2da86c4e4aceeae195a7a5fecb65ec886ec5d806fb62abb3199efc69ed06ecc681b858eb3db4762b7a51fe11d0181f80c3dd20c996de2e0f1d979a4d9b15a07c9e353f4a7d7574e600a706615aeb33c619d45be2b1d085fc88fc46ba5169972f8e43cd2e4d8778454e513f3581abe7447bc553e337f7ac5e4cc75177310bab75c33b91fcde45399ff90fea0962a9fd178e2e47c79a943ed48da4d7e0eb437782a097a07d8eac8851799d50c367839c781ef5652e7947ee2476cb3496990c6dbcd8ee99fcc6785b37c02f16c71ee096b6f28d93a9510726b622d36455a049ee7b8d88acbc9ecbe9ceb209da07db493db7f758aff57698166162edd672e00ce968adac28b33f9c78147edc7ded186ff710471445ab6e8ee28b295f6f0ea75fc204ed2fc635a877ecc4dc9aa63b956587daae5a96c23af96255edde0128b88a51881b5fbe74a1c940c0633acd15d8095b0225d00fbc03099b2f2781f3dc0b3d83805a4d402328b82d3a9b57ebd2481725ad427e955f3c03a2deb77a9de18c2a4c5985501db8f750ed7184aa73d3c1bbfa53dd47268957eb04b2ba82d34b6bfed03b74c316d21f3202d9d018b68319d219756516b4012febb1fdcd212bc1bb829ae500b70b2ba3a33440ad519a0332f5b11f3aa6d83cb31a43a41f602eac8419782a8b51895ce6c8ae14f8d0c4a6e03d90d702d83b913ace320ae27fed57095c0602dc6b11202aca93e559f58dd85aacf67e1e768de459d29a15a897a96c63b82f6835a9ea77e08f2c4f3dd60294bee0ad5dcd776ca4dbbb658f5a63d4437802615a70f6897915fa7e674c2c0283a9de096549dded09dd65793bad307b2231535a93c7d4073a5a70f6ca735d6a4f8f485ecc44ed6283fed102cab3f5de07a80f4e07bcd4a5057d89edfc82a425dc1facd06a30cb583eac1d09a15a2ceb05d417b33a2beeba3ffc26055a36e1058395a01911de155b4a1dd182e12f811243436f212f2eaeba60823cc708e7a80330ba30f787fc8a549dd000921fe5ef084cff782dca4df481a768062f4aa8e50dd811af4abdee05d67658d9ed51d4ba96b7587adeb5b9b61e8bc361bf4aeded03da8ad41ffea0d9ee960bde13bafcf065d6c23e8ce4b7cb54ed601c9925ed619b627584f7edaa8a3f582dfe09b195dad1768ff992a75b60e903d9964a3eed60fbe0ff8468c6d9375b5d98262f4b91e5048a76394b8668d6d179b961e413f08a1470c58cee3688e2fddd9502dd7344207d3035ab2d1957b9332d4efe2bd51adaa85b822110a575d2ed752c8617304318fe22156b3f0dd798d5cc515cec4184ecb3ca9084b508fbfb594aa6c77745d4f52b6c5f10ec24b3ed62bc73a7f2ea3b7a09e5d05f0bab458da3f3cb838fecfbfdc787f7df5dbcfc7a742533cb1cff8c5e1d9c1c7e38bc3e3374d726628bc3ef97071f1e1f4bd7074f0f9f8f50fc23ffe21bc16f6d1ffd08f326a2669e7f8e3c5f19de84e23013fd730b91198c2c4856f0a94a8ea97f05b887ea3aac7a747b856815b78fd1385ab6f3610d46ba18aba006ad63f878dc1ac7ed81d2d8f683ed63330ff1ccc0598db9a91d04eb490d6214f24fe4ff69c3f0ab9c1078ec82594d61fd7dfc69f093d9c46ef8bc219b8b9c059366904d2451a7d82e86b93094e0d4a4d8ce025fa80f32970b37dcf8719b824263331ad79169e4669e0df9ea00f687ec3dacb11839b43342c97518c182bc9357e512d1b0848ba914ebd458bdbfbefc2020fe752cc2cb78077164e6fc9c32c08b32f90e18f9255f98a132464f6064741423a90af872be86223b162a8036fbf171fd91d055e45786481b2d6e5b48e724b41c22e5ae4b3a67b47e0a1cf1cec0c0af0c1ee886931ef046ae23a8abd8f41c2724cc24310f55c55a4dda8a1f22e2120cc43f68b96f099475e5844258b59099860b3333045b39edc268803a5c1157c47528eee0d7efb563563debdfe96a2aee07fd9d2791cfdafb68fd703fe5b893d17276894a5dd11fd517d95bfa9bd4014304145bba3ec07fb8e31eddac73950caa77a7f2009e205dc380a6f6759dfaa6595dc9dd369304fa2c02b01968ad8fa685d5ec6603e095c1f91f53e61f0bba35a29c39b2b83cce4be2a676237ce22b5e56e320c78e0c1102d454487fb986a8b072609178871295b6fa9880dbe155e2ed06adfc7d491fd2cdfe265b64fc813a6bb23f2c4bcc43c14c1d1bf8cfdef18c781763175143f2b61a6d34a092a2324b93b6229939447f1250883ef209be3ca636592a324c8ea143fd9f757115ac578dd90bfec1b1fdca072fc6f8504662098e279277fd93771443e99fca9840dad7dd4ee3408bf2595943f882d560a7c0813dc36ac841584218da0875bcf7f328317d7c2897658b0ab96ecba45bb76d9ae59b8eb976edbc5db67f9765fc09d97f0ca454c28a1c250e10d924269d3946169877d124cdb346c555635cb50545937254bd674c54472ca546513a9a8ba61da96644ae8afaee896a6d9b6a1db962d9b9a69581286510cc9b42449b174d5320d49b76c5db70c55310dc3322dd994754dd270a9aacb9a6a299a81d06baa22ab8a6159367a942d54d9d25013e87f92aaaba62e1bb66548a83559d24d43d765c55224d350345db325c99654cb561504ac49266accd2254b439554d546b8716b2a92b4a896a228b666c9866e4996aedb2a6a5551d1d7a24f449f6e29862269b2a699e8e3516755d45b53b6245bb615dbb62c59d125d4774931644b9315cd42780da4197848a90949144d120d08e93e24ac091fcc3e838946ef3b75a28813acf421cabce594d9773083900e261ad55b4e933d87b1bac14d218ea20aa675d900e320f2960cd0c9d6711f47f1c57feb6fd1168888d6ba401a2d63439acd52c308b4c2d2f11154b3ae8645d71552b9622197e3d85f02a927e9250221528cbc2eead3cdccff46412505067e16667016c5b78700ed31e84245fbf168768ea44f70531c73a34d1188d12e9f9c789dd213d1ca1e05b1cd5ba462052ed610081e8cfc23de1360678eab6b7a5153eea032c4b22a03cf83d0d13cd97534dd301ca003156da324dd760d3c6138cb16485ce0418ffa0c10f468f77758e9a95c355623edafe9ee628e3660e959880df39742bb16b0c9c5629cc0ba4ac3de0737de3fadbabc5fba685b75055ebf0c5a556ff9de6555cda50b1e4c5fabbf93be2b29b18180e836f008ce11c12332ac1c80d243d2a637ab73b5ccc05c242845b2fd176720441b861811648c77187b832c630b9be6a406937d725e9139cc5d81bc8c22bcb809d0e4c4b78464994e652bef4b3c65ba8a753676217dc8763fc42452de1d559ecb6a9fd052bbcaeb940f65850b08f0a97d5683792aab9cc7c115d2f6b1629cff2c3eb2d2ad82b3211e414faa49669db0ea4a287899f34b56a35c26acdd0740a5b3c534cb0e5a1e37e1939f6016a4f404851cdc20be58343623c54a56dcdcee9710af3f7c6e21a0658c9678565e5d93bbe4dd27b489acc79c5fc18f9b78311aa0252ccb7c18a9f8f08afa56e188df8bba2fe7f7243b471a08e320a4bcf710e268d934ad7306921d412573b470084d5ddccec611f53474a7603627809f618cb31a5157c413107fcbeb7d082f16b10f5c783e8db2946bdf23df476b948e33a9145d222d0087e0cecfac3ea0fe20c6bf00d3cca58a0d8e4216a5e8d1ab0b715e5c5b54862d3b206e77184d4d1cb3c3e73bbcb7aa9065ebebeb5123bbac05ec0c4bf7666bdbda1d9dcdeb9e5a777e3a96a8f84e128f269d812a9d912221481c1006337c59448f4269aa0232a5f8663172a8042745e4a8de89273173d3844a63f4c389c952920b675fec53581f6adc6727ebb3d33c5d6da66cb369eb3275db99bee6292ca7f1ce21a172092fb2042d9c8b0047d4a78b644e4ab01b269abebf8f35dff3640d8a40525d5103501501d27045a0ebfed83265003df39f03f66c5d9846ee373af59bcf02d2052e43c703c9c441bbf614c661315252e74971c19cc8b50245f2c702b4f0a85cc243542adca90293fe576503340e12bcce02f17887b0cd02ab7af353c531b67f03187981f6e4a4332a76061cca62379888066cf734044d2d6d3c1631b876908a87f60ece3c9ade5e469b1025d1162b54398657e585636b3c9839d2dc30052245d187963d945593ba7ef7c1585b3011befcec8588647f2ac769c7d800cdc653b83105df2bd16e894e712d6c4294c278832f45d5668e1720dd317433878bede1baa7015c6e67d3b1ac62ccecb5b7340e04db838c046d69e3b1b886e09b53e5ac21623d31b61fd860541609626044643915d9da1d136512db19d1662dabbde6d94eebd954fbeca67f6e4b035d333a2db5d011637551dcecd47616e4a46ed5b623df92bc0ba6d347d892a0a6a77c4b727f5b123cadf52d89a9d8c0b5f5b1a8a00d88a8f9be2d5a3ed045d3950c171a5007b6728f5b927184d30c3bdbe4314b8afb36d5b86d2998759d69f832b5a6689136a8d2b2210f0d6d685bbda620c7b90d653ac755d7a795cd306d3c6e7dbe8dcbd627235b9be4672e5bd923d70795ae33d23097aff7275fe9d4d625ac3df60c20d9be6803d51335dbd544609b86a8a8c00463db3465d9b8cf43bff0926125bdcef99ced1cf46d530e979113d16a71d193334792b9e4b9f2a349f60add7b019845a1c7d503ae1eb0eac13d4dca765510dc356706d349e455e8199223b0aed882efcc77f602bfa75123a8371dac3c066a5f66c475b727a3bb35e967d8e236bba12f8ae8e15ae3c5eedaa9a80ecaafc472e1e2367447201b8e6c84108b9d32f7eccdf7ce2b71a529702733a4266243210c97a3fdfb3ffb6174a7d1c273b6d9c7dc01c8a1da8e539ae9b4a3c99588e7930869c95bfaee3c1833f1e59a819b60b6983924f836c382fa22c7ce04dff0c54271a81cc33f16410cc9cce194e6b835c271cbb360552afde7d2ae0dc1388e42182d12e61c1bf8f01269745ef179b506e50dda9b41902cb2efa930804d3e220d66d5914238608c103b097491bab5d9581150226c6878618a1d1bc638e9043b6f4553aff367600319ec50b7ec5897e418b0bfd30adb97ac31e13fff5844e95f4b83a9d2b790bea9b8fe552ab3157e62df37f9ff35202b9d0f1bfb00432f03c23e883f743496a90f169c8da1e7418fc6524f462e360beb3ce4572026d670d8183359ef72da08477cb0d701d63f6477b44a26ece6bc8e022485f3e3790cdd80b1dd9a4557f068319f225d2285a79107939a511a66c038c63574abee38c45def900d2dcd5822d65a2fad29890b65f6819905a39839219691ea3f925ad5704331f4610c11f7f088a1192bfce232223ead83088054eae9143d106a8d2dc7e869e3158d2d021192cb74b237384892c80db290f4788ffe7402d2134de5b41affa1080851acb97acbdfe0ed40c82db07f81b74835cb0cb3338cd820badfa8610deb8258842355799dff68a54b2dfc4733f6533891d2e79d8cbde4d8e86355812256bc9fa0cf10466e4a5a1b4d86049b6b5456735c8bb78e4b9ac99c76e1b039927b660a5d89f38e3d8413ece08c7f2ccfe7b2873eb62ba7ec2fcfcd016f526c385e97107567f9bec7441f9213b4fa03aaceb58fc75f47f325813fa7b369a790fe2b8ee196bf8c0681fdad1e7861e4d7b2bab49e80d5e9a6f8043cc404307c7729e33d9f81075902997ac887feb1861eb1ffe6c4bccbb34034d23c2bc87d4dc501ceda426663939920584e17d369eb0429cb388e4a37ea020bd5071807eb4c41e87a60f78e58f21d773c7268c2f40b2c13d8344d69577c44d3280f5ca399b3a49d628c89a3fa63cdf57dd7512cf48f36f635c7965dd3d1655733a10615db34fbb57e5ab9c2403da87e57d20feb79a69f56b0977a6a1f9454312d0ff95bc53729a6a6b5928a8600046152d1501b86a527299e46fe21c9cdd6e6c4a509c11936a1797b4bb8c386dcf330c2f798ac8d6b0f3cd58e5407a9f7a5ec43f2632604dd7a867c842ae28044f7c58b6906ad7033368a2f4b420fe04b0936725777445e90e0404e8e4f2272940cc21b9d9c8c6ed17f9d499f4c4d1d1fc6249e9c889eb7113e072761bb8e8374a3354131064954efe533512d8a40b39c9039213f6742a6d1579fde2ea53bf53dd34d0a133e77fd2c20458a6a005d6661f4989fc6f89dafff34e2c77daf1baf0db74c27e0a6542269d4c1ec743f5a8ca7b00582a0c1fdae2d70719bd1e2aea609fe2285f3651db86deb1715839f8b7910be8d6e3a73de8b85ef07e520be7f7bfe8455d61529d9f9f9cd430e7e2deb3a1ffc8718fc1886f01a4cf9d1d94b393a5b9ed0cd0fce589c89a302ddd63c052d5859d21d4d028633564cdd515d19e88a694972292e373c2fab34fc1c4fcb966663e3b3b2ca90f093b2a6933276889eb0d281be791684e43d395ee0470bfc68e1191e2d2c2573e24adc234d412552e13dccc2e8d1be8c0dadc8c9eb7127218b5bf912e98b06da7cc25f963d361acced92755ff19f2025825f0f0f496dfa82d08337acbbeb682564cd1a308394ef86acdb25e5a04a8b4673839a1c466d0d53351bc8c0b52ee06cfed30c5eef0c4fa21265d04617e82c996d066a76016573e866f0561778367268066fdf0dcfa662cd89436a0f86f3b1e6602d88aa61879b43b7a0ab25adbb006e4160cb49c27360ad357035317a0eafb786afe4facec18daee079e6ed1c81d9154196f53b876fa0b2dd518d23ed22953e48cfa369e0d60278e2a2db558c8abec56731c4b09eeef546abc16bdcaa23f812cbeada7cc1b7fa015699576f1c2c07db0c096163bd5164bca8373ccbd07a2361b95a47242c6beb098ad95b47d00616d711c3329bebba0e96585d3f04557657e0c87d73ee4650e177fdfa50e379fd3b9131bd3508700a980626b79b7905b15c2f2b5ac5f66025e63698cfa7b767e1174253f524e7754c350ed81fd31233dca053055fdc184795456e031dcb2db7868f30ce6d60cbd8df3650b1ec741bf858ceda1f1fcb6437c782f96d7f2c0dacb73fb2652ebcc15a5c62c81be3aaf2e68dd15538f5b6b0e54c7b5bf83216de161d4e4256e5dc78a78fb3d70561859b97a5ab18fa220cfe58e00fcadd27f15e274af1897bb59069806c7b515f990ad56f6d68b6c6fd979a95da342b776d764954b46b57aab52b75fedc42ae3cd087360aa1076f9b95580f3bd215f1f6e04d67b2ebc1db6505e78337ce4ad9076a9c15c90f44dcacfc7ea0261b84fd03b5bcac193cd0c42eab110fdb7055e778d8b62b0acaa3349d6b338fd278a6fa6cbf6d729f51d78998ee30415a3a694cd8c68a64beba99d70e26ef567bbac02ee92e9d1a2e14901e50552da21f025615d8000391e7fde033b9d80f9895acfd30b0e2b10b0656c6f581c382aa0b5c83b4e902be2c323ad1f812dfef015d65de3d105438707ff89c8df6c790f1c215085886d6c0bcd6644204691a07e3450a6984b786f3be03f28298070e0e4178054a23bcbf4bd97f62c33ff97fffacdcb0ac68aeec07b129a4d7cd42125553f93545ae220393db15620062eb4723289336e815357112c05987a2f91162bd83cad013d3c2cae5362d132681e7c190b974ae737c127213a7c72d5319a0f1ad45995a89ac26023642b6bc5eb681aeba8036c358175a9b8d5d933cdac6175756fc36ba986eed4b59c1b94d94449266086549ea83b1415464082d63f3c9c8d9e716e737e3a71b615c16709b0de2928292a1d3f52e6328af75e1ebf5c5f25ad7b9cd0691554e361b3e563dea85496ef8944c4e550386d5240623d618e9c5ee2bbc00630153129bba12342fbaae979154b338c1f018b8df2e6334cc1e8d93bd37f88b0df0ffa1bec19b94163a599a001a31bb284675a9f4cd07e7e72049a33870719a5a88967b1d79054fbc98965127d7048adc135e1f6374afebc6657e84f418a48f04df603a41ad5c4eb294bd4408e32d1d92d1d826911433ae297b83930be16202a753e1687a2928436b471d8af2501fea9558cf418a44924ba0c7d1d4abed2cc9b5266eabdda8fa6357d6603e5248e10862e8fd24fc8c10633be9ad0d161a2d8afbb570707a240877a833c28753e18df03aefc6eba1f0fa9c24770fdcd73f3cef11b78167990557f9185cc12d0e3246d77284ff739afe751f819c61be761b8497cf89925b31014d927c371fe89c7cf2f1a4a3b024bb99b13c3ffef4e1ece8c3e1335fe16393f2c23643e6abae8ddd2b32bfe80f17171f4edf0b47079f0f5ad16893ba267cb8104ebf7cfc289c7d121a1457cc47bf3c7b1e8a2453617a79f1c702894412867b6b82a613ebc4b44b3af1cc8755f64d6bacb5245cf25f3e05efd05678e246b3ad4e0283f4190ceceea841bda2a6f60d4a57594e4f60eabbd662ab346a0176578cbf62cef3d44d7412fe3f72b0222c67ff79f4c16da6db51f9f54b3e00d521262754cd6af02ed64ca1579e5131c753d8c7003b970969348531a0d9aff6a9ef012e5faa1884413a6a2cc57390104fd87da944507bd30488774f73904ef6ab5045f1aac6f677ffe3eb21961b5fff2288ff250aa80cad9b9f8445ea8b162e7935180c5efdfa1e09070c96082e407b11700505209cdfa6932814fc45484fdfd20948850057994ea1275c4f202e83041095bf8ae648887b3baf5e7d4990623041354bd048009e27204e1103611a5d062e2eba45df4b9b4540c738d03dc187c951887c8a3bc710d0b60659a73ea0ef2b5e0e5e91c9de797510a22600f68d4568b16b66f213f93e7c9022fc7119243be7b7bfa63bbf662e238910cce6889b08bfd2e757af3ce80b33ea40e9e0cf79e3050075774803590f051f225e14c31f7e22e38d439b238e9415eee491cedffc40de225a4be3688a2a50243b7e107a879360eabdc9da1b0a83935b9c0a95f8c6fcf0eaf7dff79789a20cfb8c9a2183b5981322c514b454c6d24114a35e478b741f6f08c92f8abd285f3af73c266fdf31344d728d8de1941c5d0ae4e72165f574d512e68d770671e0c18feceb5af1bb08af9b6a4e318afaddd6d88ab96375672b38414bfe81ac157f754c0e71ba2fd48758c0c963c03c815e79287c9c674f432d90ec1cc134486f8f2bd65245447bb2c52f33885da4314cddfcee30434d3165697dd81a4dd8d9d629277f8fd6610ca604e162161ee25312e23f84c5effc6d74c3c4c6f90cc683a66f427d5e9eaa7ba182edd3416fd9dd44090deb83f8c9b599d0a629cc9c3f44b9952ff03d8ffc1359836b477fd434fc0f3f3f2b1d859fdd146da07d3ded295ae5f1cc67e891664869136b9fcfce23cd8eba360e3f973c8f342d5adb18fd7c861e6986f4d651bbf9143dd214192da351f309ea3e41dbdc0b4bf7bd173ecbe75ff844af986abb62b97157fc29ba6eb32be6aacb3d2ddf3228c5fa589b0f30054f6809af9d863b97f17d4d95da2a361f9fa90e0c77b48ae3b2c7962dea3ce743cc7701b6cf6cc1abf909e6e39c60daad63d97325eb91b460b34d2a0b3e3b8f343b56eb3c177c8aee9aa2172f0e4f600a703a647ea9f78445a2d422bb325fcb8fc46ee5169997f9e43cd6e44877a495e133f3583323df917386cfcce6cac93286ba5d21d60796623567f903b0a11970f1d4440b1c0d2cab8ee77bb402c68f6125ab6f16f4ae8094ee840cc2cb0e5038cd87c384b36bd1c51224c0fe2fad40f03533da85b201d6da8255ed6bfbc0e5b1f03a415682cb7582ec05c42471ce03b5f581ef30f14bd110bbb5d7b3a3ad29865a8ee4445d0593ee02c3244ab4894e50a57298c755eb035bc4786b0f5c0b5dd719b0c3ac53b8a6b880eda189455cc716d9487feda132aa6e5afd9d609738405be82cfc5f67b81e345b0be9d719b007dde54c6739b063771c7526d21a90e435ea07d781745744f96cc1891a03a17687eb2084564438ed04d90b882584f6bcbd39906d27c03a03bb13a6cb48222da5df4062c03e309d653111553dc1d8d0c82d6072cedf8d96d7ca8bd560c962ec10c9ef2d31d2d5406b787e3ba00e2bad07ebeca7a8ad63b1aba12aa1845bf4ad1234b8a93ef53361b72c740f77167e8ee62d773125409bdd4895aabbc1963b997670b5bd4c5720b29b6909b4bc9fe90058dbd1f482cc88bc2b2ccb07bbc2f6045b5e303d31742285faeea63358efee76a0a2e53d4e27406697d3096e699fd3133a679b9dc0ab7b9d3ea09de860e57ea7137cb1e3e90e45f73c9de09624606fe86e5ca261e7d307b2172d57773f7d407b51e3ca1d502f2c9d584dc32ea82f6427926ede097585ed4a9ccdbba1aeb0ddc5d7d28ea82bd8f29ea83386fe33d485d575557c1af7461d41fb41f590e70d3ba40e80cc1ea91dd4f22ea91b5ce7295fda29b503eb29297a8b885eacb62fa3eacb94d95d533708bc6f5a01915d3155364abb315c24f0234868eaa625e4d5d78c15426d4b356a0fd9b8adea015f6cad3ac056b757bd00f116ab0be0d236ab2bf0cae02c1da1e92ae905cfb0d75ef01b802eadbc4db0742697da16ac1fe8465def486d4bdbb1eec0e596ac3b6c7d5bb609868c43774751d99ef506ef4c2babb669dd71e45bb59e9064bbd61db62e8837c3d09ddb2c6fdd7a43f7a6fdca16ae37786fca5db595eb8fa933db5aded26d04dd7909346eed7ac1f721e4c62d5e2ff87e22b3bed5eb05bab4ddeb8765b3d9ebca3efb28654ddbbf3ee0fd217bea15cb5bc1aec0e576b003e4d296b0076c2fb2a86f0d3b806e2095361247bd59f926cc6f13c6cf6c197b40a16d23b34f6cde14ee621f8123e80721f48815e2791ccdb1712764021a5ed3e87765c13c865701bc2e1d05f685152769fff887f05ad847ff433f98a3205cfc5356da7c5cca021e9e1d7c3cbe383c7e23349ef40d85d7af7fd81d2d772aefee0ccc3f077301e64e12241a232da475c81309d9973de78f689091de9d4ccec2d3280dfcdb1384195c12fb51b048a34ff4ede76046c73f0966f329ae066e2e5c402f5105a4779087b778adfc77e1bcf16e0a2e19178c2be8a6513c10aebf8d3f938293c5340dcea3e9ed651422ccb7b371348d2e6f519310ad5b17660de0a04679b31f235446dacc4b8e62708df6d43f1709d4f21707d3cb280ed2c98c74011168dedf3c034a76e290b09e25cc279fa055b037380a12f27620e4b10cf7066597b1d9eb219a25dc1244cd1f4ca9296c59865a2e460ae1af0d36db74ed833e47f43ba7f01291c0f914b8d9dee8c32c9b9e1802ef2c9cdeb206c2bb81b79fed042624b4fcad638d5d45323dddd155a0399a6a01c79280ed98866119b6e75abaa2ee8e02afb25eb2b89b098caf0217eebdc6bb2b31c596c9307e4d92cebcae3583b9fa6b2189036f4f314d49a293ce4eb1e04ea0fbed3c0e6620befd05de7e09031751c9de6bf9b5905d9893f30ae0a1c91aec0caa0d0c84377806d02a60fa97771975e83a8abd8fa82ac34448d456b40eae2aec60d45079972c05cc5b6aa3870f9ff237455cd4988d5b9bcc11e3035334b1c96d22602e7805b1a93248f706bf7dab5aecef5e7f4bf7cf3f9dfddfc34f175f076717efdfaa863012de22320d9289700a681c56e13d1ac5c1f0edc1c5f1fbe3b3f74ce5c1f0e8e0f39713f4187b21e21150b858c457f016c7067d8fe82125b840100ab28d6b1f7ffcf8e1fce2ecc3d1d7c14110df0ab2a54a83a1a19aa66ea83baa6d0c15dbde5115cd363463f8f1f8f4fde79fbf9c7ef8fc7580491d0e86f2efbfff3e3cfff4e1e418358a5a80e175e04e0643697870fafee331adebc14bf40615ee48b2a9e9aa622bba6cdb9aaa2260dcf6f1f9c5fbc1504374810a0ecf4efffbf8d3c587b3d3af83155f7e72fcf9e73304f73906617285d406289cc0d805987d30086d4bc208cf0f3e1d2088e34f5f071f119e74e19158a9210e418a50a2657889e869a8d92dfbccb46059925c6b210a2f5736212abd9a50aa4d106e21f800734b014d69bd1569c7b66d439215533687178707797b8b10ad263c6555ec7a15fb3b3045c389b41a12077ca81156d83cf5553c46139e10479e268844b935261313d5e1c5d743243f61128070a8fc3e3cf8db0744e66f8e7f180c71e786679f8e503308b4992af3faa7a83ee94406a0ac04f87271f0fef8ebc5e1d9f931ea0b9ac510c218755d48c8121a0a69348f2e63309f042e62daf3397ab733400d7d3a3ef83a40cc0a291cc22fa8d08b66c29b2fbffc20888806fc6412c51087ee1de338de88b361c2f8f2cbe185701de0a11134fbdfffd2f4d7a7b88e21fffb5fa7026272022afc0d9728fffed7f15f8528a468aa6bf80dea25e68843e13744110981bb70a31497fdb0237c48a6840a4f409823c0fd7dfbf6ec6f5f357bc7d487a28d486f68c8f85f658790723911844bfffefbee0833259649cde3e87fb5fd1ff19fbd7486d69df0e314a48eb4a7d9e817d2f1a43d51117efcb6c712a2f0e30dae416840f8f116d791b307389dce933d80f9cf8f984693bd99f06318391e629958a7c1ad55a2aec70992468aa69abb23fabbfa16bfc47dc76f6b2f91f89ea022fc813f6575b222b616e309b3bf9e09e3b4dd65ddfa2041126a19b87114decef6c950d1efa9be60c1d05804f3240abcfc25e92aeaa8bc3b5a7ac70222d997d1a68fb4aa7d1fafbedd51ad9471afa988a552f631b26b37cea26be77eb40c78e0c110e92748d8ef63d5a078286b2031894bd97a4b456cc0e4f07281d4967d2c4fb39fe55bac29ec13a90ed3dd1179625ee2b3200447ff329ecbe384dce4a057c5cf4adebeb45282ca8810df1db1b29c9447f1250883ef64ded1fbca6365caa324c8ea143fd9f757115295b0ba41fe5682d1831b548effadd0c20c04531cde9afc65dfc411f964f2a792dfa8f651bbd320fc965472d623c5b352e04398e0b66125d83ed2ae49d473dc7afe9319bcb896f7a8838ac3951caee47025872b394f5cc9797c35e72e45e70e55a7adb2b31d75a7b7c2b381cad359e959a9f610d959d9b4c39b1469294d420e1f1461176fd3360d5b9555cd321455d64dc992355d3135453555d9943455374ccc9a25f45757744bd3108de8b665232e681a9684611443322d49522c5db54c43d22d5bd72d4355f0c98b69c9a6ac6b92864b555dd6544bd10c845e531559550ccbb2d1a36ca1ca96869a40ff93545d3575d9b02d4342adc9926e1aba2e2b96229986a2e99a2d49b6a45ab6aa20604d325163962e591aaaa4aa36c28d5b5315d540b51445b1354b36744bb274dd5651ab8a8abe167d22fa744b3114499335cd441f8f3aaba2de9ab225d9b28d64a965c98a2ea1be4b8a215b9aac6816c26b5803c10b663024b962481c9619b82141c9f960f6194c347adf69c09c180d67b9ccc961e62da7d1bec31a847458d1f8de72eaec398cd5884229c45983c0b42e334862c3a5e015e4e2641f27aec27feb6f61e8916d495d8c8d96b1a15de152c308b4c2dcf1b550f33e178bb42bb45d8d857c0f848341a1ad5d8a545a2ab2c9eba23ebdc0f8df2860536191f3e12338c7095c43b772a9442f9e9adeac4efc8e743291a014c9e582380321da2cc782bb88f1ee7a6f90a57f6733a5d760b23bb2bc227341b6027999f56871134c03a4de7dc495984e6523f7259e325dc5d29715aa1fb29d3f717243c2bdf25c56fb0467d1555ea77c282b5c4010bb93bc06f35456398f832bb4d3c5f99ef29fc54756ba5550269a637afb876f4ed0ef6ad8320fed3db00653d400aebb982d903e48af6d20734f330d66414a2f8748b01544c305d88c142b5971731b58c9de1be02b0ec10f6ea077949557e3bcec92779f4078594f49b862ed34ad1b9cf2ad8e6579cd200d0d5e912e906c648b7ab4b5ef4971d1370e42ba4e0e21cee4455c5f7390ec7e2b99a35542e8e7825ce81138770a667302f819c631c0df8e63f09c80f85b5eef438876f9683f08cfa711bec9c30de39d074ce938e7b783382f6d08f33bc50fa83f68912ec0f41d4d39c64609240b50f4e8d5af382fae7d2bc37636a7e9d8e88d24580ad399bda76b8aba145dd1dbe8bc0fbf5ea4788bb5141aa70259b6bebe1e7547c95ac0f1eaa86abdb6adddd1594d63a797646b3f1d733f6c80824793ce4095ce489110240e088319be6ca7f7a334412599d260eea49143b92d29f223c4659d78123337f5a834463f9c982c25b9b80cc66189ea438dfbec647d769aa7abcd946d366d5da66e3bd3d73c85e534de39245406e14596a0857381ef91215d247352822339a1e9fbbb84340ad5d375710c7455d4a43114c72efae59b48adf474a801c5fee780352210a691fb8d4efde6b300a6c125361049260eda6ba5300e8b91923a4f8a0be644861528923f162086ddf12c92349ae14e1598f4bf2a1ba071909075f0fedf216cb3c0aadefc5489add5bf018cbc407b72d219153b030e65b11b4c4403b67b1a82a696361e8b185c3b489f437a9e33cfcd3a7a8f05510d2b5439865770da190f668e3423708148b6a5a16c5a43599769f8b83e286b2b269a6201de07d175e0a5e58291768c0dd06c3c871b93f0bd52ed960815d7c206a3298c37f852546de67801521e4337332ddb1eae7b1ac0e576361dcb2ac6cc8c6f4be340b03dc848d096361e8b6b08be3955d68aedaf636cadbbc1a82c12c4c188cc722ac2b53b26ca24b633a2cd6a567bd5b39ddab3a9fad94d01dd960aba66745aaaa123c6beb43890af6d2dc8b1caaa7d47be2779174ca78fb027414d4ff99ee4fef624785aeb7b1257f5c68ae759a204a02b6a6357166d55b6455fb16d05293d60ec837bdc938ca3d883b1b34d1eb3a4b96f558fdb968a59579a862f536d8a16699332ad1a43593187b264f59a841ceb36f4e91c575da55636c3b4f1c8f5f9362e5e9f8c786d12a1b978658f5d1f54c0ce48c35cc4de9f88a5535b17b2ba6dc9c01afb2254244fd44c43116d1f2aa261f9862dabaaae5bde7d1efc85970c2be975d6e76ce7b06faba2b8cc1b80968b8b9e9c3912ce25d3951f4db85708df0bc02c0a3dae21700da1aa21dcd3b46c570bc15d7366309d445e85a2213908eb8a2df8ce7c672ff07b1a35827ad3c1ca5396f465475c7d7b32ea5b938a86cd25b38b7ac68703e77881b178a564e29b6c68c7c42994ea663148d05a612ed1a758a624b93b2a75b54d4a575b3a1049808536ed007ba54e5124f7a2364afc60e6e91ccc600b35d93764d1527c53d42c4d16c78ae289d08486a569baa78fe5977530630c15a40c9ac65097f9b1cca3285de5143c599dcbe0c7325caef790eb4b7273378e5222cb596f3dac0692f55a896a5288f8320401ccbe2fb3383c5a360a2daa90d4724210a2b109bc7ace83a216bf6ef9734875d955540f49751bf840d454d747bf0c20cab207c706f09168872f49aa67d916f955cb63c974551fd2ffe7129d4bf49725d19b4527f3a228a286308d56d86ba7a13a207036869e07bddfb258652e0e34b97cac1e54a3112e1d12819878649ca2a7a48421da411b38125b6d1d607df47747ab3e7f17a10b3dec4a811d39ca87c2c1849af4bf656b2d9555fc70cee6008794c27e2395e7bcd20539d5c8a273dd1e05e01269269f32054bf0e8330d4ef6330e00851f91c69327a2fd48627191e86ae5e7665872a434a81889d735435ad10939badc1bfced67185c4e705c746cf782d4bc2c041ae21763e07ec319eb432fcb02fc179ffc371022da7d1ad60cffc61e147af11b7b9264a1c5ea2280a9411b99c3f0802a6e8411976db2a549a55f08e437cce1b2e864dfe1aab67071e1e7847a7b1043301026e48351df75eaec717013d0006dd9309fc5011a9fccf5e5cb7c20e40af159e6f5a1984b9e3774e8d6f5a31682ae12fbed088611095498a55abea65f873b980769ab5491e18f12716a8de97aa68990419c8d8a4e86289fb33ca61c820e668b19ee675d67daf5a3302da30f3e6aeae63231b6e0e7de4b6eed53c8f3194383539a33bb8e094decf2f6e1717c48b808dc8a086cef53e2eabe26eb862bbabe0f454df66cd1965d57b4c79ea7dbb23f96c75aeb0d45bf39d9d0af647b9e255bf32d7900ef92adfa976cdbc3e4217d4ceec7cb64db7e26dbdb9bb6f035e9b867dba2ab492b6793563bb6adfa9b6cc1e3e49e77f45bdbd36fcbef64bb9e270fe77b721fde27dbf63f79480f94fbf141b93f2f946dfaa1dca727ca6a15ad8b0adb5661da5c8dedaac86e4f955d3b52add5d9e5339da6531d5cd6b49dd91dd576f9e5e1c6da2385a25a564ee322c01487a54b84ef1f50ad9b626b5b9c19e0b8e978ab4cf69e4916ff00efa0a7349ac23c0e706473bab99de750e45534464bd29dd6b69ebb7faed8070d274da3a60928dee6a1dd29aaa408f57e8e36fe0113db62165dc1a3c51c6dacd1749f461e4c6ae30c5c1c1bf310c739af44e12491cf0f71b497cb2cda47ab13bf5f2f93f75957de8339413b68395b603a8daea187c092b70bb4356b90de5eb41857d3ecac4593850429f090686c39ae71144d5b632293d08e949aa6777930cb69aecd64793a484eeef20fa7b7a5629e9000afad8ff868233fdf63b30f1626521ee9361bff069f9b34f6a34860e1b26fb3657d1ae11ff9d9464314fd2a41e0b4f1f4c437ebfd6778931ea3c2faf4d3b656dcd1d5a766b45c9b66a8a74d5543c8a02fe9ff652fe88bf204672fe36bc8c13d4d6cb3fe838e10b3c3f93f9ec707a5d10bf91c2633cecb20382669cfcbf8209a80eb097f4bf6d82c2b49a2c9ca5d2429c9af039a049310140a6b76323fba03bc0226df0d96b3d81c44b91b84e1633994da160a318b1c46bb1b864d6f9c41e9eda1708ae30ccab81b2acbef9d0198cb0068a3529dc0dd0429a6e97984b4d35aa02f5c74bb765e6915c41b73e5b6d2b14604dd018bb9ed02c4ce6e77383cbf5da0d819ee0387e7b80b5c36cb6b407048e38669ddcd7458769eb3a2b5130d2b2109c17c3ebd3d0bbf90ded7d37437a3db144d4104fd51b024b129164c20fd71b0e4b239164c3cfdb164a4d416010ef55da5202c92701e0d9c48ad9a49232b5d4b588b30f863810f04d15ee992dcaa239615a5e1623aad1632ad10468d3acc54a87ee2cab657b429b56953eeda6641b20fd41e4bdfed9a946a4d4afd9ac48be181be915d390fdc245e660fd464b626b73f894423ac2f56a60365e4d91e4b195b9b9028b937f39a8672c77aec02582caa2e40eccae80e87c9bb0b144ba37de030a17581cba86505083be50dd3bb26a47461e903dcb44611990c38a0be0438ade3e0108457a0bc73f97b9e2d536cf827ffef9f15b5744573653f48b247baa51192a81e2719979c6133716a2c4490d07dce05b58ef2e2687e145d8783ca684e17b3b07aba4dcb8449e0793064362d8d449fd982950e9064146b87917760dc02a662596c84855d275b408417ce4668d895b4154478696d84285b6b7d70c80d6019a957cd606b34c9ac0c6601b0ccdb0b682e16623b517515baa6654b29926bc5844934a3d9c5d40ebd924db0e1e811881fc533218da63006f42e699f1e31e0f2a58a4118a4a3c6529c3b3a4bce2a95086a6f9a00b1d7f31ca493fd2a5451bcaab1fdddfff87a7874f0f9e0eb5f04f1bf448146f2ff4958a4be68e1925783c1e0d5afef3f5c60dbc25922b800cd25b8820210ce6fd309e27bfe22a40c309d8054087095e9147ac2f504e232480051f9ab680e43e8edbc7af52581e805aa59824602f03c120d1d08d3e8327071d12dfa5eda2c023a0e5318137c9806715223823bc710d0b60659a73ea0ef2b5e0e5e91d9de79758063b4036c8c87d0e21b95e427f27d78ad0b7f5c06c9cef9edafe9ceafd9c950220438d67b2afc4a9f5fbd421c5f98dd3ab84f0efe9c375e0050778702b91f1d0a997df50f3f91f1c6d729c25e5eb893dfaebcf981bcc5f9e8e2688a2a50243b7e107a879360eabdc9da1b0a83935b6c36488ec07e78f5fbeffbcb4451b1f72683b5981322cdedbd2b652c1d4431ea75b448f72fd1bc60eb508f3ed3368ab72c482d91003deaabf084bc56dd558eaddabcb76e0fd81ea0dc2fb7a85cd98f76a84f769e2dea5776deadeb933d76536d3a4dec8cec120be3b3f073345f3f4965bdba167ee7347502ed02524c55bbeaec647583c0d3d50e829db02e1078ca56d4c79614f539da8de122811f4192fe379b6abb405d7dbd248647ed019a66b43b7857b07c663b8030b3db030acd70072866963b43a199aef81a37cd6396c886d8d8905427a50f43097b9d792531d975e05500af4be57ebf71f6b0b946bd22930ce77330678dc7f749121b5448eb9027c6b267377fc4914d10b6c959781aa5817f7b8230834b626d001669f489bec5d78099770b12a9b85ae60642b4bc09482e1a5c4af00d106345911b115c7f1b67ae334cca7b266fc9a7dc8ee0a20cb59237fb314265d4cb272b398ac13552637e2e0e1ef21707d34b6cfb3399553c5bb0578494bb6d90f5991c3316f7cc27d3c4354741921973e4227d6f507619eb8d994153804d5c0ea6d38b6a59e93093879161079b4d1e53fba0cf5942186a8f412c9e26d1146df63eccb2e98921f0cec2e92d6b59b31b78fb947612c7352dc57365cf5155cb73340342c71e9bba63fa3244eaade4fa9e8c7316578837d33b13185f052edc7b8d934b8a481b43df12bf26fbded70c6b782de06c8b7b24d91a9d65764e05627f721e073310dffe026fbf84017190792dbf16321987d13bc0c3397e7632a24743f6068ff50fd464ada608a31e5c47b1f7112d0c6601932d384e285c598ca386cad45b836436ce9acb0cded864c7bb4925076297acbe3ca72fcfe9cb73faf29cbe4f3aa7ef6367f45d9fcf776d36df76b97cb791c9b7671edfde597c3be6f05d91c1978c27b3db684ee84814059a731029845809281e980b7880f32856ea2d15bd62bc20c3cb05d24cf6b13ccd7e966fb16eb04fc4394c7747e4897919a4530c47ff32661e637c8aefe2247ec5cfcab5405a2941654488ef8e58594ecaa3f81284c1776ad6b33baa3c56a63c4a82ac4ef1937d7f1521a508eb19e46fc56119dca072fc6f8516662098e2831cf2977d1347e493c99fca4968eda376a74158b590c6fb9eeaf12684096e1b564e42f34c9d24d361f69319bc3869cae9dc4ac5e14a0e5772b892c3959c27aee43cbe9a7397a27387aad356d9d98ebad35be1d940e5e9acf4ac547b88ecac6cdaabb9ad592187cf826e7852f89e49e1bd600643722b4a6c8e67e0e686a786ef3b9868f4be537ba6180d67b9ccc979e52da7d1bec31a847458d1f8de72eaec398c55db8e7a2ef45c6620dd23f296bc3356265fcff6264b09d8337fd4256ccb49d8c96d6885b9e3db98e67d2e16695768bb1a0bf91e686f30d8475bbb14a9b4546493d7457d7a4741628630ede167610667517c7b08dc09bdf4a0bef2e7480e0588034ea3e8db62ee0c04f4c19790e6b93fad5e767ab7685f1ab8785b45881363259e921f322bbfc4d154441cc0741dc31b7b8ee629ae63a17971c640533c1518b6229ba86590b8c083de119c426aac8c6f600e2bfd91d99be4acb1a64e2de6098cd3b3109b1d2c45e12a80928bc5388175f56ae94a97383ed7bc5257e3a0efca496c187b7a5a5f444cab5ca3d1abb6a637ab0dfc903a2c129422b9ba11672004973878d3228e8963787e05c1f4b20653b91f4dd82bc115c84babbec54d300d90664d269de95446b45fe229eb338bfda8986e7cc80e5d881100d2ab2acf65b54fd8f53aaf533e94152e20eaf924afc13c9555cee3e00aa4c42c29ff59ba49b1dd2a98025a5ef4be939863865ed54ed1cb5dc3680d803607ce04608b4577315b4c3383fb5a00bb5990d21b3812ec0d719177cb96adb34a85e64669603f7cc124a08581164d565ef34727ef3e81f0b27aacb3928f35f130ec4456c7b2ccbf768bdc14c4066a813bccde037e4f8a7bd57110529e7508b1fd107506c940b2ebc4648e560df5d527f7a759b438309b13c0cf30c676b0d4739b4d95f121bc58c4686f0ecfa7516643fd3dcac2094acc65ecde80c632a357b81f507f10c35c80e93b6ae85459299888c52cc284f8e70aa470e7a7efae49e6fa3801007904f12d46106f0af82783b1849431287a328920ae2aa2e52a6351f2816d00c94002ddbfcf5c729b05fbdb56a8bf2d05fabbf7307f5b0cf2b7dd107f0f17e0ef3ec2fb6d37b8dffd849d5f9160c59086bad92b1afbd642b13746f5333640b379ecfa4d09f83904e8ef1bca6f9b81fc1e2a8cdff683f86d3784dfc305f0bb8ff07df715bc6f7ba1fbee2f701f4fbdf064922436ed3a76d7e4bfe6398d5e644e23d7763555b64d51d3a12f6ab20a45a040559425db9734c5702d68bcb44c85fdb5389ed5681b990a3569a8a9434d7eb1c9a17956a33fb5686d129fbb6b72133f88709d9186b978bd3ff14aa7b62e6025a069b6227b2250754bd414df106d0d18a221d926f425c307b67e9f477e38c0f266a77cce768ef9b6288627511c7cc766b1bdb3b5df9760af90bd178059147a5c3be0da01ab1ddcd3a46c5703c15d736624ff5a859e213900eb8a2df8ce7c672ff07b1a35827ad3c1bac212c2edcf8cb8eaf66454b726f50c1bac66d7f38c174d91573a13dd64233ba66905885e168324cde230d1e6a658a224b9cf2ff5674e4a7f663a1009c988403b50c9485d4f81bd459551e207324fe74046514ddf92c79238d6c78aa8a932d217810b45433525d3f574690ca4977420a3d94353192a8a3eec914c989fc76c2dcb74df44df0fa17169fc3c860bf51e427d4968eee66972596749ac0392d55a89e552c8f73202449e9a3a33323ca2f11f2b523aaf4212db0a5982e77a94a715c9aff91dcb0b15e9a60fc69a67faa2255b16be63f145a01a8aa82bb6adbbb60f75df7d49221de3e82b4eb844df9a44579fae40efa1ea7189ce253a2a69169dcc8b6a3444ec049d6fac317bae0aeb2c95208e588ac8eb73733de2d3709366ae1e2413dbcfe96c9a6df4c3f4171887c4b01eb1ed190e7585c9f467185c4e32e95cbad86461b248ee110afca1cc341f24acb74286fc8264b9479da3e90a69b0b7b7df2e63ef90e69bcff86cc96f31d43b300b7010b06ab67bfaeeb7ac677ade02d27e1025629f882c26599db311f5c2637a4ad020a17281b3db8797e59187f7369a7a45952f5885c283414a1609c4de3e3888414a5d45a53c921925aabdc10118084802865ee1e59177907a6c9cd373b5da2067ef206a3f4cc1252ca1a8de839f3e426c5ac5f6d705f3007fd1f7c2f383ac002f0b93fd095e2ea6009f74a2a9cf865a9786f4ff2957452fce30429c4a52ce9ee300f781622c2f29eafa968fa727a844ec28c96c4c120e0af44ffeed096572b4104783231da68fa711d639a8730829785b19c212115385ed382df9bf686cb36f97158b055b4f1cb45e418c7870f2ffe983c60f9c81e49b80ffa93789cbee6e10d7a22b552a418a07d66b267fc90c202e42734c7807cd4888792a2e6546a4deeb3170d182c32110846402e6f02dd1a77ea361ac49765154881bfa9ff2114fc9aa155ad4ff5b599fe2cc20d0680cf3ff95109fb2ad4bf1f1b4147841f0b76a3fd60f20adf6dfef5117894421cf2c41d002a29f308869416521160d3674e97fea902d3ad580a6051445ff3ff5b124f3d3029ca1424363be69f94b8b75475ba9acb2ca0c357665191ff940b678d965764b9bc11973aaddc93ea09b7ac3132ab7d704dbda0fb4dd16f61ffdee16025bb512b86b33a928f650b694a1aeb5dccddc8fb9c03677aa77ac003788dd69bfa1ebb5ebbdf79def1677bff7b403def62eb8f54e78735cf7394b1b6eb9b76f5670876981d21bc53d8e624f1383ed9b19ac175edd047d7b61b30d61df5ddc6f53e0df316a1d84fef21947f339c796d53fbfb047ed702bc055bf0754fd1a6e0dee5ff1bb977b813bd5b9fa6ef891352fae31e5f2a37ee8b035a5298c9e98c65419c46c47de47b0f7540db90c7e2e327877549efdd5a3744e80175d0bf40f3ec959e054233bf4b00e1565477974c9e78778a8bce56118aac91e37a147729ace3c5344efa7d138cf7ec274859e2a61fe404b99c32a232f2b739f54511ee0f30054ae967dce0f07a51dc3aefe675461ebedd2deb4f95a0a7f1424e9d2c71c607d27cf6a523f9cf5b8c542b602d6dcad25ecc5cba81e4b2fbfdb5abef2f249b86f21862450f55110b3776f84163de822cd744ab35103cf6baa731d83f9e104c4784017093c0137d8f7f82349ccfb2e8a0f1669f41baa42e67d8e93da3421292e7d0e700c1bd2dc14fae952d5ff9ca67f255a30eaf8e962362647fc12cde2835bf958260c8ef1edd112827ddc89459290566af4b64bba3743bab640ba42f661cc65c4cd218eb7ece18f25abe80cdfdb890a5a48111ac178111e65c10348fda85832d9ed44ad52b998e67180f316a105a0974987ded3b47d51ccde79e4f64dd9222661d3f28e1ea2ddc23cbfff62fa8f669a08c1bdc13b9215f13cffc86c3af04f9cb729a13b06c23fe94767497dde673dcad39eb56071793fe93792de1e9198e2c9805eb1e2271cd09052191a11669c964620ef0dfa88b22f7e907e08b31ee6dca3618e3e8458292577ab99fc38cfc2fc6779743f7f1a7efe387cfb69f8f6e3f0d3f0e3f0f305fa79f16969bad67fb09b856bfb4022ddd0f947f05330ff1984de944ccb395e68617a46cb4b98df265136997f2c8077968564d31a06022f314a560555663175ce6b33a9b024905d9e62068f46e2e6ac0cfa16c33904294b91d91c788453d33b699c198ab9fc90e9db3b08a082787ddddb951da23dc83f6d79bd52333e6c04f0fd03fa75436fa24302c16425a34486e4ed4792fd2bdb4887e8136ece831b38cd2f934986b008f152e0a23d1b89624faffd617c49022a24c5adf5471cad9001c6820e7f1ddba89c959d04f4ca88ac1e3417e720a652306f8aa90a6ef045b494c503ce42ee154d2ce698ba90e40cf32fa1348f38615ea01058f27541c874502d9ba3b42367211719d8faf0ae9339778ade4d046f7bb1bb0da1db2472d708dcddcc9e6495e1499fe1a1477ee795133fb4b2a113f94e1002d7c5762394ae3a0e5e6170b1bcc5c28ddd01ed31e90c077f023dab297ad9e7e8603ac56bb68cd74362d6e74d8ca3687ad73866673d1db6b9b5705e78851e3412097ac22185f019215623ba222ec2c41428b18ad57034f99f7f2ca2f4af12fd931d52d207392bab1d57565f9607975544a41bf4376d34af5f1e6466887208f27df437ee3dfdb58f7bbd6a9788dfb10b9282203a6660d951a1c5f867d6241d19fab0dc8b6ca8e9c36805b6b23bebeb61bc0dad9694ddaafd9cb8c9ef75c3c21e5652046574cbaca579f92e9f82202787bf03e88e75d50522d4b5b1a8f9962602e849a22b2963453135cb73ed7f6650f981679530b2a3cf4a033d676c39fe65c3484a3d27308f87d980925e86f7c55bc49d6cc0acff55d9186dedd4b2a195aa42b67983b8b186664e4e7aa26e884bb9c589bd3bbee67d0dd9aa789b5b1dbbe5f89b5b1cbbe2a8be012739b4ef89b73cc06e406c4843faff8aae6f827fe572c647bd9b2026c7d04da3bca36e8c76eb04b2f5f5f4a04be89e560d130e748b23b3146af33e713fd00434870fdde65c3484e5bcb7716b0a2ffa302357861bddead8ad0b3fbac551ac8523dd22e6a6ebebedcc08ab23f39d43e79d4376d1467e66976b1df79eb320a4c7f5cbdb622f5aa0ad426b04ecf962efd8b5cda8bf74bc916fbad67f174733ba79effba5753cdbffe07a0bdbfa6e72b6b8f9671334f7f9d5a4816e1fdd7c6e989d0e56bddaa8275ae9ce963baf1525947936a611597b9458fd9c5f49c69c8bdbd01d81ec182cfb18f4a15326ab4b73969395b8d214b8137c048ff33d61b81ceddfffd90fa38b86c873b6d9c7fcaec4a1e7094e9911aadd39e44ac4f3499446dbfaee184ec92d4a3209e648a8dd04b3c5ccb9c277060c45ff3de3e7305cc4d0b181694b006a8e6b798aa379baeb007fac3b2a189b63604baee9ea4ee6791878d9afc47175d9576ddb742c0f223057539db16e5b8e6dfb3634745503865c82d1167f12b4a1407ffa31849368ea39592fa06a9a63d9571ccf7535479365d5b14d1f38d6d80492861a82be9da35b2cd87e9896e2b9b2e7a8aae5399a01d1178d4ddd317d19420f4aaeefc92c60d1939e238c73867fc3da7ba159c5f08f4510931b24074d22197262905c9ee7aaabcfc4ef6e08c67114c2689130ca1cf0e1e502c45e31c7b506e50dda9b41902cb2efa99c7e6ff2116930ab8e14c2016384d849a01b85de666345408912375ea429d24009769c8bca49276899624aebfc1938271576971d656eb3a57b6d7155f1114dc88a9b99acb12c895f91cb4ef8c73f04e1f54fc26bfca3eacfdb293fd4d205c06c0c3d0f7abf05de254c93911b2dc2b4f3275f8198647fc3eec6490943a290b481fb6fdcdc3ac0fa872071b64230ed16174b38695cf95024b3a357976fd95a4b65959c7f991912ce515779ce2b5d90d06987208597517c7b1480cb18cc3e65815c048f3ed3bbcd9fd1bce374c2b3818064571c20aa831f892f325e7acce7665872a4d9156d7ee17c421c19f6067fa39ec8f44abd7488242ebb8501596611f6179ffc371022d6e532f74cd68bdfeb2ca6ca1a997d080c0fa85138b1342bdb644b934abf1008e3c18a5ead6a0b17173915516f0f620806c224f76fd7691eb9839b20c96efec980553ca1bfcc4b838ffc2a5f3197d2fbd1a15bd78fb72059bab6471cfb82f6318cd06f6a0581ad8ee8d7e10e221ebb5c45863f4a24777166ff439da541eed7ab9321cae78c24fba68d616e9ddfb7571614be522f2f43858478ee23ee152d2eb351a65ea6d80081758c67926fd73df687d68e3a14e5a18ebdce99c1084a5ffc71e6715f5ddc0551d3e803b851b7f629e49975fb25a48d6ad630a189bd68328d7c84f4743cd4c656426db44f57e79a860e6dd71455533645cd7015710c655b04482d03ea58937cbd7d2cc27e73b261cabaed25addb5adaba07485cb7d5d475db4e5ef790e9ebee2781ddb653d86dd3bfe5ce34769d3d63b796c7ae5526bb5691a1b69acc6e0be9eceedd1b686bbe40db496ab7ddb4760f97d8ee3e52db6d3bb9dd43a6b7bb9f0477f797e26e9b49eeee33cddd5a4f51ee53bb6d9fda469fae268faedd51d376667754dbe597871b6b8f148a6a59394db99e0795630cceb178aadbdfcb1663434f76d0590cafd2d7a36ada5e3107afe4a9ff73a5556f38691a354d40f1363ff8a7a892c269e21c6dfc8322d85e0c67d1153c5accd1c61a4df769e451637ea6697c1b71050f27d0fdc63a51edbab8e4300afde0721183f677319749eeacf21e3b4821248396b3453c64a087c092b724fe5adbfbb395688e6bf6c7771a30afc44426a1e5954ac3f42e0f6639cdb5992c4f07c9c95dfee1342ab37805b17b065d5be4ee2e3fdf63ce4a768b3c0c1e8d06c7468ace6e628a5c0da8fba4521ec6b1ddd50593ebc1cbc0f15972e268aa61eac0741dc31b23604f711d4b9314670c34c55381612bb289cf8ce2cc61ed204922370074cde0dd79a70b14727e1d784efbb67390cce1eb348b6592e2e3e19089669914b8fd6ff016f3afe80a6d84e25fe02db63e4fd2cb188310241fbc6e5f8ff9ff45b488b1a30f76320ad08fd753107a22d222e200c6af05d4e6deebac07af05423de7488b01f12deac0973020c75bafe5d7428a093cb79643281ce0cd82cce46127b3ebc35da33f6b477bf810ed13f499b97c47cfd58a4162a8a4f6aa727a9e53151b69bc990469a38d2b217f29b8ecdb4cb09c46f847758ec8c559f593a017a4f4ae215b37d80df01815d6190f6d6373cfda0fc949eec2d9c54da28ee64b0271f4d54e9e162b3c6697bf6c77540e0b3b6b64b47b8c7e4e02eb479e5cba7cca08e3be86ff00736832039b8c3ec172ba9896e39fc68b8e388ecaf3eed2a5a4587f4e691bd5d92de81dd9c91e77bcd86fc2f40be664f9f146bec43be2a80ac22e6caf4f43a7ac0297b1e93e78ce33f65dc157b2f13e2829072f3bf7b08cbc6b974f23ff309a2e66619bcbf82604c46df8ed2d59d51b2c13c4e80e237c30cf1ecef458b2d57e144bac779c81fbe79a955b752eb41e7af8c7f8e2182d3b623be2b8d83394cbaf56f28bb9c97d5212ac61463713665e34732a4813c7d76da8aa86e2a03f48b22963cbb180243bc0b70c19dab267daee1624db72cbcf51cc2d4f486b898706000461521177cb83f2a4645f3f86790fc2af32464f5800861156df9e9ee4eba5c43c1fc10762089c09e02ac7838f3c313374fc389add31f84720859f8319bcafc127e7984eb89982806f8e309f71e6d17c31df00911724f329b8756834ab52067aa39393d12dfaaf3383271352c78731892727a2e76d84cfc1f18cae63b4c1d8846e29c62089eabd7c06049c469c7c39f93e43f25dcc3d6cc0e980940bbf471bfcf12d1ffc071ffcba7b079f82ed4c41f6d87c914b4ef62a2e8ca424b75567efac84a0b0a2c8ccc547abc08a73fa0c44be1ba4eabf93c12977c3351da864d0eaddd0747b9703687703e41b931c44bf1b84d1a87328a32d1452637218f36e184676e450567b28c4f47228fb6ea8a5d55acc75037dec8e6a74b68bd491203d8fa6815b75a09de3a2db46f2a3af90e6961b86543a57052c08b00b509504bb4036116117f88c0cbb801484d8058825c5ee709818bb40b1e4d8070e136417b865922ca0a9210a3d8262a8b2910a7733b315962cb3a246ba844caad08100e6f3e9ed59f8857c43cd1ba88ea6a0d2fe28aa34db1f4f1305f7c796d1737f040575f747c1d2faa65830e5f7c7c1ae83cdb1e055d11fcbf21a698b6b77545f1758d54ad218046165ad94a58dcb6511067f2cb045736e4d85958328c53be56a21839dc872d451a642f52b97db2cd6d6527b529bf6e4aeed5517e20335dab46a1fa8e96c89b76b4daab526756dade0070fd41ecb3c1eb449cc691ea841962d3d10d1b03cec819a5c6678db6f986ccdea9c90e90b13eda03d9fc437d824f5d4cdbca687af61765d80aa1cab0b6413dbe9029ff18e2e200503e802c4aee2ee707829768162d7531f38bc28bac02d53f60a68963c1b4871b7d40008e6d2a0bef45da7d17f1a34e203f2825ca50f0e417805caebeabf4bd97f62c33ff97fffac6c12573457f6835cbfd37310218962e67372c38f7234f38b775c2fcb1f819de2096a7a6492e59af6e2687e145db3f63268b8c9dd7b35963c29132681e7c190390729565f16e740d5b532991f1ad09aa5fd2a340589676844b90f966c6d6d84a3c287b68129e3311ba16a623a1b2164b9c316106176b1111a967f141133944d30618eb2519796594c1f74720358b6c2ab16f9b545c7300466ddb3f2d523c961c0943841573c46a2eb7a193e74c2454c4816270fc4e18f6d6017875ed8b7056b04d40e087be2d4411cc00473891798899c9e7d26ee4c4e0e543bbdc521491e3710c9a896596a77d4304af4b49e96560548f358a3318d70b69e52ac9450f86c1a5fbd0969348531a09e73fbf4cc1a972f550cc2201d3596e2e59e10a32e1c9b68c59b26403f98c2394827fb55a8a2785563fbbbfff1f5f0e8e0f3c1d7bf08e27f894817f482f0f2276191faa2854b5e0d068357bfbeff7041125c25820b10c1832b2800e1fc369d2099e82f422a1cd30948850057994ea1275c4f202e83041095bf8ae63084deceab575f12885ea09a256884337921818ea4b7308d2e031717dda2efa5cd22a0631c9e8be02331b1229fe2ce3104b4ad41d6a90fe8fb8a97835764b6775e1d84a809808dbc105a7c719dfc44be0f3347e18fcb20d939bffd35ddf935bb6a48846036475255f8953ebf7a85b4016146af971dfc396fbc00a0ee0ea9afd150c8a249fdf013196fb2c0f6f2c29ddc97eccd0fe42da2b5348ea6a80245b2e307a1773809a6de9bacbda13038b9c54152c8fdcf0faf7eff7d7f99282ad1adc8602de68448f3e8569532960ea218f51a8785c45c87fca2d88bf225b5e498bc7dc7d03409ec328653a262d0783ecb690849d6aac0831fd9d7b5e277114da65459cad3fccd76388ab96375e7285960cc8b1a53a98dc921ea24d23b71841b4458609e40afd4d9d86c69574528a9e3ca8154e13348e4c8248a83ef18256a3786a99bef8333d41453969b9badd1849d6d9d327e9abb6b4a1022417488e3be916631ef9fbf8d6e18a7cecf603c68fa26d4e7e5a9ba172ad83e1df4962c4d94f0cc69e19c68ae3552505852287499f759511b7ab8678a783234b19a2a1af82631106833b94dd359bf626e72f47b80717f225cf9ceb11f350dfe03cc8f72a74b119fa4479f24b5a5e3119faa479f2a7d9da93c9f9f4de6679b7a8a74df7a0a099f1bdf0a9fb3e8d0ad94d74fd1753b65e51992516f85651d19ddd32236eef0ba78a02978526bf98e6968b19eef69b2ccb51e067caa9ed05489c56e00bd0a62e8f1a9da78aa96ea1462b216d6edce5aadea3ce7b38377018e57c80f91eee310692bfc415beddecb0f68ee62062f7ef99ec014e070677c093fe1256cdfe1b3c5d7f1dd42fd5e26c6bac393914fcc9f9ec17e2679cad6b1d75e772b9cc796f4f0a90c994a7fd09b739c740847fecc4b1d7aafdd6efa73a0bdc1a3659ccbf2d65c64d7f6f8c6fd1cbbc6af24c82c93ce47107ac2d9358e723e09e6391d3ee26784b37c86f0747006593fb6a574e9b82d4239af8abd003ccf71b1d98893199a38d7413a71c2c8c90d4e3a39e5565aa0a9f14a5bd70b80a351a3b6f0ea4bfe71e051839577b4e17f1c411c3e892dea98fb142f4fc70fe224cd3fa6753088dc76a53a966be5d1a75a64dc36226959a8d52dbcb01458720bcf26cc75d16ac6590ed245927b9f652098218d56c1156e6245656965e5e24eeb8f05c012aa5d138d8e712de0b0eee3cc222f40855ebb0e56cc075b3542626ec6f032480835b7025976006bd1b5fc43a8f7590b80dc03b0458f6a2e8ced2148ceac4e1041cb99c8aad7c7e74efc3498590780cc83b405407e90d9aa72e958daa272c53db143fd714b8aab3862b6187cc6e5b2093be5672c33a1f2ec2cfc1ccd3bf09712685d670a0ed3aefa328fe908c770997690353ed30ea8ca69dac1d4784d4ba0256ed30eaec26fda81641ca75de52acfe90493719d4e3081d711a0cb5855784f2710ca7dda8114fca75d752663419bea2c0fea06d19e40583ed4050273a215f53305b6c27a7663b84890029ed070034ba8abaf59ff9f652635ea009d31aa0e204bccaa0f6cc9b03a4057995607c00ae3ea0057655e5d00eb0cac032ccbc43a805146d601a0c2ccbac35186d61d2ef07a00751d4396b97507230cae0358cee43a8034a516b9038461763da0ba1113c3f43a4321c6376293a334b1359242ed88a60523c79865dae512f63a4ba45e142c6581df6f9ddb7d7739857c817606e69f83399b03777f77440b691df2c42428dbcd1f8b4c54382899509e0f5c7f1bd33cdda7d1fba230cf614dfd491769f409faa82b131c4e921e3a91f44624cd183e5e82f18719b824872a31ad79169e4669e0df9ea00f687ec31e9a9223992cf1598053611d4ce999495936102620b968c8cb8d03ac658777c03b0ba7d544dd45ea6de62b70b2f7bdc151906459a8e8c94296c2a918eac0db6f9f46637714781582c91cb6b6937e6250669e18ec64f93668beba9a5f18c27b1dc5de47240498a5408e51109d5c55c87ad45099a66ac6d449bf3dcb75470a0a77b84ae2aa640ed2004cd1dc26b7095a5b3869d93b128c726ff0dbb75a84c7eb6f29ea06fe972d9dc7d1ff6afb24d71afa5bf1668c1334acd2ee88fea8becadfd45ea0799ea0a2dd51f6837dc71cf1ede3602fe553bd3f345d1c70e328bc9d657dab965522f74da7c13c8902af04582a62eba3d5771983f924707d44bcfbe46c8e241c634b9993a9ca2033b19d98fc4d71e62298dfec32e08107c3148be9781f9369f1c0049942aa152a65eb2d15b15e5fe1e502ade97d4c1dd9cff22d5e4cfb843461ba3b224fcccb209d6238fa97b9ea1963e77d37dd3f41dc17fbfa61b2c78e7e840a77d064e6152ae102d24a092a2384ba3b62e9959447f1250883ef209bf9ca6365eaa324c8ea143fd9f757115acc782591bf95a45de00695e37f2b843103c1145303f9cbbe89233210e44fc53fb8f651bbd320ac2600c432b252e04398e0b661c5cb1586d4a113b79eff64062faef90d7758c6ab16f2baa5bc7631af59ceeb1774db25dd6751775fd69d17f6caa54d28a1c266e14d8a5662d394614987af134ddb346c555635cb50545937254bd674c5d414d5546553d254dd306d4b3225f45757744bd36cdbd06dcb964dcd342c09c32886645a92a458ba6a9986a45bb6ae5b86aa98866199966ccaba2669b854d5654db514cd40e835559155c5b02c1b3dca16aa6c69a809f43f49d55553970ddb3224d49a2ce9a6a1ebb262299269289aaed992644baa65ab0a02d624133566e992a5a14aaa6a23dcb83555510d544b51145bb36443b7244bd76d15b5aaa8e86bd127a24fb71443913459d34cf4f1a8b32aeaad295b922ddb8a6d5b96ace812eabba418b2a5c98a6621bc06498a3a832171ea26fe3b48ef21b6957c30fb0c261abdeff4163d4eb0c28728f3965366dfc10c423a9868546f394df61cc6aa65410ab1533f98d665038c8368d96c9b6424dfc74125f0dffa5bb4fd21a275c98678191bd277961a46a015968e779fcd1adc6e9ee354c8e538be4dcfb3071129465e17f5e946e67fa3808d5441f4f82338c7d9a443b7b277a5fbdba637abc326a15da548508a64eb26ce4088d4c0587017714c124867c193d880433598fca02cabc8ecc357202f83122c6ed0ce0f6d96482624a653d9c87d89a74c57b1cc6545e9874ca725f72bf2eea8f25c56fb845334e775ca87b2c205447bf6495e83792aaba03ddd15d2d6b06293ff2c2356b3dd2a2813cd719eec75807f572d98bc3cb8785e03b8ee62b69866812ccb630192067c16a474a74b36d888860bb059a5b8b90db40b45d388779c821fdc40ef282bafa5a926ef3ea16d00acc5a159b1769ad60d0edf5dc7b2bc66903a06afa895040e16b2a81b5e7d4fb2fdfe401807215d27871007daa0f1673390eca82099a3554233699104ed341cde14cce604f033daa903fcedd82ce104c4dff27a1fc28b45ec03179e4fa32c26e1f7c8f7d166870e28a9145d228efd918455a7670b1f507fd0225d8069661cc15a649305286689e7c53f577ef53b3f1d733f1cca068f269d812a9d912221481cb4a19be1d3437a6445831791290de64e1a3994db9222625fe5c493980df42ef831fae1c46429c985ed33898d5fdf72a03e3b599f9de6e9ea178fbfcbb47599baed4cdfbab8fdad8684ca20811cc6ef0d2e021c8c872e9239299168a6f1bf1b48fe2bba278b40f6c7a266fa86688d3d4dd480221bb2093ccf32fe3960cf40856984af4b1a2dc17aa4b5092e43c703c9c4413bac14c6e172befbd693e28239916165a2be3f1620ee9e11d15d246934c39d2a30e97f553640e32021eb2c108f7708db2cb0aa373f554cdcfa37809117684f4e3aa36267c0a12c76838968c0764f43d0d4d2c66311836b07e97348cf73e6d1f4f632da8428896a58a1ca31bcc291603ae2c1cc91468b2b13381ada5096d5a1655123ce3e186b0b269af648874d1091907ce538ed181ba0d9780a37a6e07b25da2dd129aee5f824adec065f8aaacd1c2f40ba638804355215b789eb9e0670b99d4dc7b28a9128d25b1b0782ed414682b6b4f1585c43f0cda972d610b19e189bbb6f302a8b04313022b29c8a6ced8e893289ed8c68b396d55ef36ca7f56caa7d76d33fb7a581ae199d965ae888b91d2f4ee16b3b0b72aab26adb916f49de05d3e9236c4950d353be25b9bf2d099ed6fa96047ab2ae48b62f5a06d045cdb33511981a145d59973d19f8ba2df9f7b825194738b8b7b34d1eb3a4b86f538ddb968259d799862f536b8a1669932a2d9b434b1e1a6aaf29c8716e4399ce71d5f56965334c1b8f5b9f6fe3b2f5c9c8d626f999cb56f6c8f541a5eb8c34cce5ebfdc9573ab575092b437c4369d8a207a02b6aae3d1681ab48a201544f930c45d30c789f877ee125c34a7a9df339db39e8dba61c2e9d9cd16a71d193334792b9e4b9f2a349f60add7b019845a1c7d503ae1eb0eac13d4dca765510dc356706d349e455e8199223b0aed882efcc77f602bfa75123a8371dac3c7a415f66c475b727a3bb35e967d83a32bba12f33d890c3b5c68bddb553511d945f89e5c2c56de88e40361cd90821163b65eed99bef9d57e24a53e04e66484dc40e1924b26c9ed8eb9ffd30bad368e139dbec63eea8e1506dc7294d72dad1e44ac4f34984b4e42d7d771e3705871e411ce826982d660e8994c3b0a0bfd374659d228a643e124e7b4f8c1c8436f693a0f5fca4348b365c1e65c7f08f451043422f38070bfe46c2e7cb1368b5e0ac98b3756c08c67114c2689130a7e7c08797488ff48a41ad35286fd0de0c8264917d4f85ed6cf2116930ab8e548093f720c44e82b35b799b8d150125228e8627a1d8b1398e934eb06b4f34f53a7f0636cbc1ee56cb6e57498e01fbc8acb0b8c91aa3e4563877e599f93a99c3d43b066763e879d0cb52128d5c1c46aaf3e75d8198d8b69da2a7a484210e706de08833dc3ac0fa87ec8e5671fddd9c9b5180a470433b8fa11b30d659b3e80a16f9d04f71feab9ad91966b1381e0d74abce11c495ea900d03c3d815d65a2f6d2389335bf681993da298b9839561a33e925a55e7e818fa308668a57ac4948cb53ac4347bd81c8f26f34dac44abc15e6c0976c2c33fd625fa2081662845e4f119e14d4ad244d508b49a4caeffdef84372820624a032ac7d38a13a9a2f09fc399d4d3b45245a71f6b0fc653464061d96aa616135ee5ca791279a1f1ffb871efb1908426702c6410ad2e5c41ccb734018541ed0e9be26e20007dc2273b1c93c102ca78be9b4756cab651c47a58fd3a02a7d18efa716f2a709f73b62ba71dc51c76cc2f40b2c638f2d4f68576c84bb96fbeb08e9410ccec49125cd1d6b868476395072b4b1ac3b63cf751dcd97141b6886ebc271bf464f2b0755f586fbe13ccf2cf62bb87383fe7e282f88eb407990b3c6ed7869365a382067f41561d8843eec1465d521e94979a7917f4873a3b6d06d9b1090fcc76f6f0933d890551e46f89c9ab561ea81a7da1176887a1fb93f90d873a9cffd2de7bd2f80f736cce8e6cc97458ad47bc71d4bd05064d9d114497534d9f71ddbf20c47417c59f75ddb001ad812fbad35fd1c19f0f2946ccc816ba3c27970130fae8cfb1366c2cb191bf8bee3c184df1cfbe9b95cf6bd18d95799d02d89be1c27927963d3403b0e080cdfd12c5973c68a6a3b32f00c4d95c05857b56dcabdacdd672bf4d8b9d88ecccb303e2981d77d69dda3bc6b4b325b66bbd963e301e82e897256b90b2425824ffd938bc09a59ba8dd2666bb416845ed4d69325af066a3896a867f2bda3c59a5e5d4f31bb1a3a8be45acfd5744773555656cf92ca7ad0d7467837994f83f43c9a066ecdb91617dd2e0d3c2dc62c9f9c8553b21aad87a3a3df11b2610afab45d9b878e28b2c9e8d3707546d663c0b17e1a666137bb7060a7252b5a9a1758f1d407f3f9f4f62cfc42a226d683bc36a1c8827df746d2305b1bf6a83671fdb1e5c1d837eb4e753adb22c361a7aa5388b53f1caf0cc990a41ab12c2b5d9ad94518fcb1c0e658310c2fd309e55f511a2295ad5ac860269c067592a950fdc415ed5132586a516ad3a2dcb5c5069a79a0969b08ec819acea8b15d6b52ad35a9d787564977fbdf4934813a4d339d612ecddb513cde3490202737f39ae05b4bb65dc01a68af73ab3502ea029f5141e726ab53b9029c9d8f86b15f13baa7486242cd941ae4ce017941f4f6c12108af40a91eff5dcafe131bfec9fffb67451559d15cd90fa2ec533d5348a26a3c1ac6ac821d965cdfc7d5890e4e9d00dc320dd4054df2e3c5d1fc28ba66b7e7bb34275735bc0a2d132681e7c1b021f6b140cc4569dadddc0d0f0d6bcd7e622d164abf1be16920e8cdfb55a3f0cd115e07532fc471deb785b0ba2036c297adc93e38e406b08ca6abd63035fa62960043e92c0bf548a051b4ddc334cbaec738baa6654b81ad6bc5841b34a3d94dd228865ec90fd8f85e08c48fe2999046531803ea2eb34f37787e3db9112e08c2201d3596e2b9c982114b2582da9b26406c4d3a07e964bf0a5514af6a6c7ff73fbe1e1e1d7c3ef8fa1741fc2f51a0a1d17e1216a92f5ab8e4d5603078f5ebfb0f17d8ff6796082e407309aea00084f3db7482189cbf0829a74b272015025c653a859e703d81b80c124054fe2a9ac3107a3baf5ea18d3a7a816a96a091003c8f849702c234ba0c5c5c748bbe97368b808e4312d27642239ee1a8b604778e21a06d0db24e7d40df57bc1cbc22b3bdf3ea0007bd02f88800a1c58779c94fe4fb70d873e18fcb20d939bffd35ddf935db972742808367a5c2aff4f9d52bc4da85d9ad83fbe4e0cf79e3050075772810abdfa190e5dcfae12732ded8524ad8cb0b7772c3a9373f90b738486d1c4d51058a64c7477b5192faeb4dd6de50189cdc62df697246fdc3abdf7fdf5f268ad2560a3543062bcb79872968a98ca5832846bd8e16e93e5e50e4579e022c2b5f9231cb09bbee3507dd93488bde9c7bee3ed36a4af79d56f302898356398b3f45d73c9fe63a32b8a7d4b8d23ad3459ef6f1911216cbebcd1af9bc3cd2bce8edae1e9fddfcf412579bcccf63e793be77c1774853b271c9f764259fd2ce7c99afe547e2b56a4b1b473e418f3441a2bce61489cfd07d8bc3adcca1b6c686ed4f2b70ee9cad15b9c985dcab969ea9e02c6d1f3c9cc18f963af4c463d06ace72a03c6fe0bdbbdf6651772eb2e31b7cf2721ecd17f3951434a55f504d6ff800dd0c67f978e3c1e5242b54ced99c8ad964cbf0f3001f503ac0f31c171f0a3ad9312262e9e9c40923a74c3ed8d6d4a9160e8ff8fe96975207d3e9dbbca843ac7bbc6e1c3f889334ef614b13aefcb0b13a386b167c4e610d5702cb0793bbf5e4dff5c4d10d56182d7294774857df689ab1ed9cecac454adbea35fb8cb660b50ba9ade55c6f98897b48e4dd341bf792a1ba730af38639e900589b9587c83fbd3c61f799b4b861e2ee37eb6eaf14cecb93d815b83a915bcda7cbf484963cb07650bb0f26260aed6420d17ec4fcf64a9c63dda7afbc0b67620ca72de3bd2c59ecee8eae2bb6baf973b2265731434af7948d78759ae0a674c3098e1c88ab157989e5bbf30157d2fa96798e8933c5799e44a148d8f3290fe5903580b7a379b31f235446dacc4b8e62708dc6fee7c2702a7f7130c52e58e96456cf429c1bc7649c2e61cff8d6e4262eb336975d6e932cb992c1795de6e5ea077dce3221adcbef5c66595ece950c6ff0b5b36302c585aa2f3963d3361d0d68d0018a6c39602cf9aaaf02201be3de29933159be1670a6d03dc53425894e343bad2d322acf1763a417e7d994b37e0f843778bc7f7890dcca599bf79062f9fcd3d9ff3dfc74f1757076f1fead6a0823e12d22c9209908a7809a8808efd1e80d866f0f2e8edf1f9fbd672a0f8647079fbf9c7cc5ae0d218e41245c2ce22b788bcd16dea3b94f092e244705d9c6b58f3f7efc707e71f6e1e8ebe020886f05d952a5c1d0504d5337d41dd536868a6defa88a661b9a31fc787cfafef3cf5f4e3f7cfe3ac0640d0743f9f7df7f1f9e7ffa70728c1a452dc0f03a702703b47b39387dfff198d6f5e0257a830a7724d9d47455b1155db66d4d5511306efbf8fce2fd60a8217a40058767a7ff7dfce9e2c3d9e9d7c18a2f3f39fefcf31982fb1c8330419bb5040a273046c21ab10a0621cea788bb77f0e900411c7ffa3af888f0a40b8f98718478d38050a225771984a879bb659f99162c4b926b2d44e1e5ca2644a557134ab509c219041f60ce288074a91569c7b66d439215533687178707797b3800e02d9eb22a76bd8afd1d966e0244721f4ba7a146d85ef3d457f1184d7842b44626049128b7c66462a23abcf87a0862b4d202100e95df87077ffb80c8fccdf10f8321eedcf0ecd3116a06813653655eff14d5279dc8009495005f2e0ede1f7fbd383c3b3f467d41b318428825b49090253414d2689e6702460c7a8e53cded0c50439f8e0fbe0e70c23de809bfa0422f9a096fbefcf283200a3886e8248a21b62a1a478bd0431c0d13c6975f0e2f04bca1462b51b3fffd2f4d7f7d8aeb18f2bfff752a20ce29a0c2df7089f2ef7f1dff55c0b1d9309aea1a7e837a89d9ec50f80d514442e02edc28c5653fec081f129a85fb04843902dcdfb76fcffef655b3774c7d28da88f486868cff55760829971341b8f3efbfafcaf8fe23feb397ced0ba137e441b6447dad36cf42b0ad12f51117efcb6c712a2f0e30dae416840f8f116d791b307389dce933d80f9cf8f984693bd99f06318e19887c9eabcf28aa69a2b53cb93beaf4e2f8f3ff0a7acce1d89e6d733e12e69e8c95075ce454fba8a3a2af3a4f43d93d237a69f7fb279d9b9f4e7d29f4b7f2efd9faaf47f7cf97f970670870ed0560bd88e1ed05b13d84017e8ac0dacd407c8815465374bf3728c78de739ef7fc81f39e6305cd81093e0042cb0f1faed13ce832cf83cef3a0f33ce83c0ffa13cb836efbaa6a5a86256ab6a98b787723daae2761ad47577ddf576d57e179d0791e749e07fde9e54157747d28ebd65051791e749e079de741e779d0791e749e07fdf9e6fbe279d0791e74b42519db1a1cdb401255e02ba2e64b40b42034444d1fcb96ac424531959794077d43358ee741df46a2534b213970658327425fe5c9c013a13f67e1ca13a1f344e88588358135b6c6be27cada581335cb5344db04bee87a12302dc9d70dd7fef32542df5010f344e85c3f78c6fa01cf84ce33a173e5ed7967428fb19d490c63f14ac98437d9cc8e89230dd5cc6290a459285bdadc148b942477e1a1ee4949e99e44072209b0c8a61d606fd3298ae45e9446891fca3c9d7b62601abe05c6bea8f8b22d6a2a70455bb3a0083d4fd23445350c13f07b627e4fccef899fe03db182f6344871b5f83531bf26e6d7c4fc9a985f13f36be267b8195ada6cecc6514a3640ac471cde3b93a9ae98ff17fba2d2ff1d66df9759681f2d1bd5175548d02d2108d1d8045e3d1450518bdf4fff29eea755cdb53d083d712c0345d44cdb146dddd2441940c390a007bcb1f692eea7b33874fc72fab10e9f557d48ffffc91e3d1bfc6e9a4bf43e12bd5974322fca144644256cf45a593b0db53dfb024c719ca9e578534558471c4e6785c34f2521751126a8455ee0266f9c6ac9ee68d507ee2229127ad8d90cbbba950f6c629b450cdfb2b596ca2a9e8a677380231561cfbacaf3abd22d938483a23d4c8af050e7317403c6ad6b165dc1a3c57c1ae0f873a7380353cd5f0d67aeba8287384e52920d0f1d9da531ae8df09ac1dc1db158d9a806b8a0920b9851006b9f548e07094195b596794b8a7934af22e6ed4752ab1a32b0382bf788631beb13b9322971d73cf14b493496f3c3774a093f7aa47cf7f52077cfee2b400c813301cffc2b967c539ff7e7b081365fc22ac9a378be846fc933c23fef4f59903ccc9ef3ece98b9c5b3cd1cfc81e1b05e66e2dc77c63c67aaa52d77383dd91e61e6f10ea89ab5683e4fc7f290fcb6a90e54000f51c21ab612b9c6d29edc11d5f56b091a504507700e66b368733ee866316480e65de0d5550630e632dc32035af3af5bb8d89edb10d2b2eba6da008fa626f50a8a8951e3580619ae80254504517a065bae8025da58cce5f58d24667d0823aba40b2f4d105aea4903550381a4c0349e409a61b724e371009acc4e300f3f9f4f62cfc42ba5d0f2ede848446bbe88ba220a0fe2896c9a93fae2a716d383225a96d88a820bcfe785832ec8fa524cab63870bca52a2dee3624b8bf2b1bfd220cfe58e0033cb4ddbcc4a9a1b1c08ad270319d560b19dc44cca06e3215a43669e1313d2fb52735b527d5da93bab65710ff03b5b7bc521ea8e1eab27aa0466b6bf0215b2d16ec0335caaeee076ab26405db6f9028d2751ec1f48139b26ccb4170d43712f3ea665ed3cbd6b0812e40c55aee02b4bc20bb40575755e72f2c974667d082bebb40b244da05aea4b415502cb93490c69a3073457a20e0a6356acac4d601bd1fc751f1078720bc02a55dc7dff3540362c33ff97ffface8f32b9a2bfb41a2e667093092a81e2b0d979ce1ab4f7a83ee9649cb2e68222b2f8ee647d17538a80c28ce659ad4526ce0326112781e0c1b339b08e4568ab57f2663573bd35f8b07afa08db0144b6a232ccb6b6c2374d545b7f92895ab70735cc5b2dc0815bb4e3742542edc3e68e406b06cdd542f436a04ce2c336635b152c40b68405072655ab5a5b9a6654ba9796ac584e334a3d94dd228865ec973d8789708c48fe29990465318036aecb24f8f88fc7a6a325c1084413a6a2cc53980b2c4195289a0f6a60910fb52cc413ad9af4215c5ab1adbdffd8faf8747079f0fbefefefbfe728b95bb38fc224fc993dfc555cad846a2780a6ea345ba7f0943888d7b3cfa9c677fcbded6b3b68d1af2878d5664ae1af5c98934e2797b78de9eede5ed496188733316897b54a8f99ee5782ed41c4d93340700cd73a0a49a0ad03dd375953e897ba2310954ec6d2f6f4fa5df0f95b8a7d228cfdcc363f7f3d8fd3c763fcfdcc333f7f0cc3dcf27738f8b0381b855ba26427c77c4ca72521ec597200cbed39befdd51e5b132e5511264758a9fecfbab08694658db207fd9373eb8c14982d0bf155a40dbf929dece90bfec9b38229f4cfe54369bb58fe2098ab892c3951caee47025872728e2098a3a27286a1272f858081ba599b669d8aaac6a96a1a8b26e4a96ace98aa929aaa9caa6a4a9ba6162d62ca1bfbaa25b9a866844b72d1b7141d3b0240ca3189269499262e9aa651a926ed9ba6e19aa621a86655ab229eb9aa4e152559735d5523403a1d754455615c3b26cf4285ba8b2a5a126d0ff2455574d5d366ccb90506bb2a49b86aecb8aa548a6a168ba664b922da996ad2a0858934cd498a54b96862aa9aa8d70e3d6544535502d45516ccd920ddd922c5db755d4aaa2a2af459f883edd520c45d2644d33d1c7a3ceaaa8b7a66c49b66c23596a59b2a24ba8ef9262c896262b9a85f01ad640f082190cc9c13331d29b819b1b7c5ac707b3cf60a2d1fb4eefae63349ce532274797b79c46fb0e6b10d26145e37bcba9b3e73056afcfeaf9ab729981748fc85b32605e99302bdb9b2c25cdca82712d615b4e9c458c9e2bcc9d279fe3c9e7ba269fcb6f327a26a1cb93ce293ce91c4f3ac793cef1a4734f2c98a00c4cdd365557742ddf1535c9b2454bf181084ca44aa82e44ed583c98200f26c883093ebd6082b2260f6d6928eb3ce91c8f26c8a309f268823c9a208f26f88c43abf3a4733ca81fda92e8966c7ba66b8a403514510363f44b32755176755bf724d7068af79282fa6da8c6f1b87edb482a83af8db5a12cd93ce91c0fecc793ce3d9078e549e71e27e99ce46abaec5b920814534222569645cbd654511d4b16d44dd980c0fdf3259ddb5010f3a4735c3f78c6fa014f3ac793ce71e58d279d6b4c3af76931859f32e4ec6d7abcc076ba242ac5dfa16abab66fd9a2eae1abc4b1a6a37dbb618986ef799ea2d8b626817f567d2031b8408fb0f706028dde9cf95c26294817092d12f604e1f5f1cd3c88a1f75ac83b4eba9c356df8be6e69ae08806a899ae58ec5b1eb41111b4e293a5465c3c579ea8947efde20c354bb30aff4e5f8e3c571d98e9cb7a36992e59a50164d0fc8a2a6ab9e0834d5177ddd772ddb1fdbba352edbf9185c5523ab8dc870f19c7d7fbe332d45357d4b1e4be2581f2ba2860852b4810b45433525d3f574690ca49774a6a528fa50c6197aa5618f8c04fc486b0b2a2b3b054f5465d5f89116d78a3a6b455b94913297915c463e968cd4eca1a90c319be622f271446491ca894b482e215f727e469e76914bea2d4a6ad30763cd337dd1922d4bd464d5a7b61aba62dbba6bfb50f75d9e76910b6a9e76910b6a2ea8d70bea3f6ddac57ac7e06c0c3d0f7abf65614fdd6811361861e33bacd16a2c5720264ea6a7e829296188dad2068ec4695d07f894b3455e04d8b0220b2f7a7b14804ba432e5d73682479f6974d59fd11ce3d01933a48ae5618af3dc8a32ab01665872a4f46e83041c9d2175ed845cc5ee0dfef6330c2e2729f6840de145f01d66315c11231b03f7db658c63c5101d726ff0179ffc371022da7d1a9715ffc6cea07af11b3bc566b151ebb289a9411b99c3f0806a944442946db2a549a55f08e4371ad759a2af56b5858b0bd76dd4db8318828130211f8cfaae53bfd5839b804698cd86f92c0ed0f8645ebc5fe603214f907e9639b02ae69213311dba75fda8c5d0ad04af3d8261847ee3a051a43c8b5a8d3b984799ad5491e18f1289d31153464380c620ce4645274394cf591e1417075a9e2d66b89f75656ed78fc2b40c642c2488acbe21fa40f370998d328d0e8f53352092c4df4b8a994033686a2e848b099c4e85a3e9a5a00ccd1d6b28ca437da857062348d1ba74698fa3a9574b13c146b2cfa3f7e3b40fd54f21cf670c0d66d776754c686297f7358fe30ecb65f35664737bf758083c59365c43b42cd31535431b8bc0b0d19e478340934de002a8b4dee9f49b930d5d64b7e724bb3537d9077094ddaaabecb69d651fd25df67e1c66b7ed32bbbd4d730bb7d98e9bc92dbacdb6729c6db595dcaaefec16bc67eff9a8616b870ddbf2a1ddae17edc3f9d1de8727edb67d691fd29bf67efc69efcfa3769b3eb5f7e955bb5a45eba2c2b65598365763bb2ab2db5365d78e546b7576f9b06985e5c4a8693bb33baaedf2cbc38db5470a45b5ac9c867882298eb49b08df3f1479a0b1782ace0cb2b4d832d97b26592827bc839e5253cb791ce0d42c74733bcfa1c8ab688c96a43bad6d3d77ff5c619c1a4e9a464d1350bccd73d350544991abe61c6dfc03264cd72cba824572df539cbea936ce38edd5153cc4495b2a81c5491a974ad6f0564791bf5e26efb3aebc07738276d072b6c0741a5d430f81256f17bedf24bdbd6881f664eba784419345372bf09000b339ae71144d5b632293d08e949aa6777930cb69aecd64793a484eeef20fa7d7b8629e5109afad8ff868233fdf63335c1626df1ee9361bd26f652af80ed9ee8be879ebb3dd7f8637e9312a7c94acf71d3e27cf27f882be86f0bd97f13d7310bb70fa72e627fb1e92ede2857c52e6dc4144e18bfa22eaaef2b2be693e01c9cb9b26b4a3c39951eef8ae235405a7357c26df8577ee2fe8abf2d4122f8951e459845fc6d730c9795f06c91549829ff0e7648fcd9b03d47fc4ae59ab105292df7f9671ac83625b9edd3f8e56c114e9b33310b92508dd4b6740cadd40a5e29603a9ad81a87694c36977c3b12a480ea6b706cbe47c0e68b406a4c2348733bb34984bac1cd86a0d5c88851cd4be1bb4ca7b8ba96f412e459ef41ca605bdb049be73b0161453a6f4ce811a286677545b13bbc97c1aa4e7d134706b71af71d1edf252a1e568d5e7a722951e55a18ac5d219882e972e60cc82e901962d992e909545d303305f363d40b385d3afd162e9f4002f174f17e0daf2e9025aac9f2e40ec02ea0257aea03550388952c38ad9cd8e98d82594152daf2158c97800e6f3e9ed59f885f4ba66b653c751aca80d51d0f5d51f09b3da364692adbdfe782a2b716334f9badc1851b64ab7d1a162cd6e8cac5cc1fd51d5d6737f44c5eaee8f825debfdb1942bbf2d0e9c0baebae0b1be8b13ad0661850994a5cb7c6011067f2cf004c730bc24868948624769b8984eab850c6aa208a25e3215aa9fb6dc60c13496da939ada936aed49bddaa31ce6815a64d8d1c3b698f1ae076ab4c2e81eb6cd9c2b3e6cab190b7df04f2df8edc3b65c32e7076ab7c6c91fa8d582ed3f507bac8c78a0264b81b2fd06c959495dd2307d60dc575aca216c6d4e127fddcc6b3bcc35c2a4331095085dc018b6de032ce3cd5d202b0cb60760ce257b8066acae5fa305bfea015e329d2ec035ced105b458fe5d80d835dc05ae5c882ba0d8d5d4b072d6e4222cfc29809bd6165ba61b1e5057721228ed108457a0b46cfbbb94fd2736fc93fff7cfcac1cd8ae6ca7ea4d85c839ea30a49544fac874bceb0973075c92048e8e1ea05f541f1e2687e145d8783ca804e17b3b06a4344cb8449e07930640e4d4b7e92b9d994b1f2c8d0d5ec3c56a12918cce65828c7d9080fc382b68127e3491ba1aa30a96d60cab9d63670656c6c4bdd2af8da36f0958c6e236c35ceb711ae82156e8485e58d1b212a99651f34720358c6abaadea235a6c2b0368683b18a8d17d02cecc4c580e5b371744dcb46f5ebad5a31e1f2cd687693348aa157f27936112d02f1a37826a4d114c6809a5ceed38b295cbe5431088374d458ea62e3419268173bb5ae78d30488839dce413ad9af4215c5ab1adbdffd8faf8747079f0fbefe4510ff4b14680edf9f8445ea8b162e7935180c5efdfafec30576c19b25820bd05c822b2800e1fc369d20c1e52f422ac1d20948850057994ea1275c4f202e83041095bf8ae63084deceab575f12885ea09a25682400cf23f94f81308d2e031717dda2efa5cd22a0e3308531c187c950887c8a3bc710d0b60659a73ea0ef2b5e0e5e91d9de797580b3b202ecb386d062c3c3e427f27d7e1ccd843f2e8364e7fcf6d774e7d7ec3e3111029cdd35157ea5cfaf5e21912dcc6e1ddc27077fce1b2f00a8bb438198110f85cc0df9879fc87863ab43612f2fdcc98d10dffc40de225a4be3688a2a50243b7e107a879360eabdc9da1b0a83935bec5d472eb67f78f5fbeffbcb4451718b2683b5981322cddda22b652c1d4431ea75b448f72fd1bc60274a8f3ed3368ab72c482d8530bd23ae71a9bc5e3dd80d5bb97e38d8028439186f519b3d7a6e5f3d3f646e01519eb1b7ad9c9da6b7a8be7c0adb1e283f4f6e0f513939ee04464fc0db83b49fecca816d8bfaccd16c536d4ad12cf1ee129fe5b3f07334bf8b9ecb9af51dfd1a8a6e0754d274bbfa0c557702c8e8ba1d4c41d91daa53da6e07b044dd9dc032faee04c35278574042e39d80ba10004be7ed204a4a5f511fbbb4d4497b37868b047e04494ac2742ca1aebe6e503d47ed41aa0ba10360b1183ac0940ba23b105d141de0f285d115842c8e0e40f505d21d942e92ee70cc42e9058c174b77c0ae84c22c9a0e50c5c219b1c1fd9a9605f1b33ba2be63ef30d2323647097b9d45db290a96c202ed33b3bfbb1c35a8009c81f9e760ce8642405a1e2da475c813e3a7b69b3f160e49d826af78401bac6f631aaee5347a5f14e6a14ce8c1ce228d3e411f7565828df2a8af19f17221de669368eac1f8c30c5c12479c98d63c0b4fa334f06f4fd00734bf3966a2b290fd53e6ff16608fa883e9f4a25a36403b89e4a2213c0bb62fcc7cab8077164eabf15a8a082ccc579cd0408c41923923d18d65e6c9530c75e0ed5f5d3b645a1014f94c671a45df1673076a926adb26703c59f71ccdb614072863c58196e9fa9e6978b26dec8e02af4224d9862c81f155e0c2bdd753107a22daa6a08f8b5f9313bdd7d135f6559d047367b108bcd702f1523a8f83196291bfc0db2f6140c2a8bc965f0b99de829138c0435f3bd819a0ee66aba44034a0de8db5cd206aec3a8a3d1c828a5913e41c1191d35585be470d9569600f4cacc510955da76394394b923ac5b638aea465407b4db497415491dc2668dd61afb77768fb01d096f8b76f3513d1eb6f29ea19fe972d9dc7d1ff6afbc4590ffdad04c48c1334fc6853437f545fe56f6a2f10854c50d1ee28fbc1be63a2e7ece38bfaf2a9de1fea6f08dc380a6f6759dfaa6515d3d7e934982751e095004b456c7db46e2f63309f04ae8fc87e9ff8cf118f35b6948d7cc60e32637054ce0412f7943a4e600a30ad30e0810743b4541181ee63722e1e18732724f451295b6fa9e815130f26bc5c206eb08fa923fb59bec5cb709f502b44db4af2c4bcc40218c1d1bf8cc1e2189fb4bb983a8a9f4c83415875a2c4c2a07ab40361b28ff7be95532018d2b30d3c1df94fe6083bae2513e940c9ab68791d35afa5e73514bd9ea6db52751fbaee4ed99d697b257593a3b90aa7a1a1a12aa2bc99e4f1875e21ca8d857cd691dcda9f47497a8940c83793d7457d2a3348209551953916b1cc2a8a0055169adeacbe1442025c2428452225c51908d1ba89057711c7c465db5b8e285c83c903c8641599915881bcbc095adc20218bc40f715c653a958ddc9778ca7ab3629b7fa61b1f32264036d3f2eea8f25c56fb849da2f33ae54359e102222d7c92d7609eca2a484a5ea1b58d9741feb3b4e767bbb59b427c4e07a6688ea9c646aef042af7ab7e5e56e0c790de0ba8bd9629a596dd562cacd82942a154497417a710136ab1437b74123ec61192ef8c10df48eb2f29a633879f709f14d58e53e18315a5df46f45b8841e61605ed5d7a18e0531d56c484a6acc935e9153d605ee30abae7dcf5257c903611c84749d1c427c4249cd3733904c2b4be6689550a7f932d5933b05b33901fc8c542080bf1dbb50b339b83e84178bd8471ae6f934ca2c18be47595cbf22b15674798b6369868517f907d41fb4481760fa8e1ea5b2fe8264018a59a807f1cf15d1e0ce4fdf5d9321fe7122f1f118e35b8c31de14794f573545362d5bb4a1e9899aa5f8e2d880ba286b3e54255f1babf27d6681df30eadeb662ee6d29e2debdc7dbdb62b4bdedc6da7bb8487bf711676fbb51f6ee27307d53b65c9c7a4cb587b2d62f93f2d6c2b53706d8333640b3797cfb4d49f83904f1ef1b556f9b31f51e2aa2def6e3e96d379adec3c5d2bb8f487af715476f7b51f4ee2f861e4fcff064f22f37ed3bf23d094e90c3f31ebdb83d4953de231be8da581f9ba262a99aa8e9b225dac0524545313445326d7bac982f29efd1a67a1ccf7db485dc47963db4d11448cad34d7ec4731f71e1da5fb83609d05cb8b287ae0f2a5e67a4612e60ef4fc0d2a9ad8b58a02916542d5fd45c732c6a9aaa8b966a49a2a579baed9a405174ed3e8ffd70b4e3cd4efa9ced1cf56d55104fa238f81e852998a2e5e2a227671e054c762df9d1447b85f0bd00cca2d0e3fa01d70f58fde09e2665bb3a08ee9a3323e9d02af40cc92158576cc177e63b7b81dfd3a811d49b0ed61596116e7f66c495b727a3bc352968d89c26bba4df72f2ca5f89f1c2c56de88e40361cd90821163b65aeda9baf9e57e24a53e04e7096016ce68ae10acfe57ff6c3e84ea385e76cb38fb959ac43d51da7b4ca69196a7e15e2f924426af296be3b865332efc4dc72066e70ae40e70a1bc4322ca82f726c42f70d5f2d14c7ca31fc6311c4243f849365267408c72d4f83d5b699439b1a82711c85305a24cc4936f0e12552e9bce2f36a0dde9138e0ee466710248beca32a5c60932f49835975b802ec8788103b09f641f5361b30024a24ce7891a65148b1630319279d60bbe668ea75fe8c2793a9b643cad77b49b591f5967676e9936b1fbce6db76472cd6b5f93b46cf39e744c554fd8e78be2f23e3c44bc934f14c3232bc00bacabe8404c87de69f92bb5fb54893f18cd26338d829fee57ccecbf812ea50f632be05fb9b64f6242fe77bb095d08bf89a56f9645e501e9967f22938c2ca73ff9218840952c05f8c94992fb0cf4d021d308b1668677b399e3ff32f6a9d5ce5f96521a9eecd9e6e2e924af7da6723a1fa748f6c24952c267aa72c26740d77cf4852c9efd13e2149a603754f46c2281c1dd291d4a57b978424cd395bdaa425694cdad22631092b27d6e52659825be6ca05780b326ae28105bcde2f0d8bb1c58c2ab565ffc4f3aa74ef6475f9f7c9acb2514e968c601e32a34bce067a80b28ca02f386105cf25110de5069d001bd841277a6ce207f79056668304313596f062d2c46cfa395556b279a2982d269cc908f1a924afc959d0c6885886b41d64843dbdbcec3c94916d80a681ad6db0529a98dcc366d7d924334e8d01f2fc38dbcd56b36254efb5cd8c693f70869cc74acb932de0979f0f2817340fdb2a2b951ea16522c278dea57b6a944ad2876ab341ec3e14636c92d12f2215d2d63213d5f480279e9fa87b27abb2b14f86a28d721b65f4fe9099917279d1039465fa7dc109e77e2e099d2823ec04d8c0cd3ad163134bda727a269e68696b89966aecf129254ada428faacc712ba992b69b75295b604f2b8953ce60b7818be5b85bc34758f00b4e5745b9f666981ad8f8664ba989aff3dc4e85e5c0f3cfedf43412078d1a92af8c56a4a818f160fc7fa260fc53889a64a3f1cbaa0c104f818ee6c9aea3e986e1001da8ce184abaed1a966a5872af68fcaebb9883d0bddd381a7f8ee8e1a2f1175de7d1f879347e1e8d9f47e3e7d1f879347e1e8d9f47e3e7d1f879347e1e8dff6946e38750365c5503a2ac4ba6a8f9d258b455a88840f181045ddd5534c8a3f1f368fc3c1aff138cc66f1b43431aea268fc5cf63f1f358fc3c163f8fc5cf63f1f358fc3c16ffb38ec56f6a606c788a221a60ac899a042cd11e4b9ae8d9ba6b48bea503537d51b1f837d2e27824fe2d44da953569a8a9434de691f879247e1e899f47e27fd191f80dd984f6188e4553939180f580225a9aa48b1e92b0b6a1cbae0efe8c91f83712c33c0e3fd70e781c7e1e879fc7e1e7aa1b8fc3cfe3f0f338fc3c0eff9f290e3feba2c443f1ff0942f157ecd479287e1e8a9f87e2e7a1f879287e1e8a9f47e27e91490578d87a9eeb81e77ae8ca9421f66626d5c8973df3af42fb09788d36b42f4574b2dfc3130bdce3876c9458a0bad3e489059e5f62814acc7cab6f3e02bb775a00a96f5a00799314081d920a2ce74050dbcf495dc674c92eb0ccd1bbe416a8f0cf75d905da242530b79894a0c632785282679f94a047a0fd8db3193c4a4681c74ca1b0cc4aba2069e0257dc19f5a46831a3fe1190d784683878eafbfe5c4084f2e01c153cdd4b0cc13fba36ce090db41f69c9223d478294f8ec09323f0e4082d2356bfe448f68f9990e14f949ee0cf977b6259823f50fb0de2fe115ae64919d6ea1f3c29c3b34fcad023d1c0c6d91c1e25a3c263a69058e6a25d9034b0c2bee03ca3c3d3cde850e3ad4f2413c39f287dc236a2f76f3f11c3134d75f0b4f3432c33dc8db03670609e41a13091e019145e4206850aa3e749149e5012856c6600dafe4996aa79c0f01d68f8b2a349bae400d3008ee4bab6e6ca9a64aa4a9fcc0959131ba64d403d7c888409656b3c4b02cf92c0b324f02c093c4b02cf92f0e4b3242c5fafdf4fd284860d1acda2a0f02c0a3c8b02cfa2c0b3283cb12c0ab2a9d986292ba2e279a6a859ae248e5d133dba8a0254d9d5159f6751e05914781685a79845415587b2a50c6559e67914781e059e4781e751e07914781e85e71b3190e751e079147098674f974ddbb7445db6d19ec483631178108a3e705ddd577ddf30c72f2a8fc2867a1ccfa4b08558c9b63e94556968da3c93c2aaf8be3c93c27316ae3c9302cfa45088587b6c2aaaea2be2d8858aa8d9c042bf145b34755db65ccd555c5bfe136652d85010f35c0a5c3f78befa01cfa5c0732970e58de752d86a78f5c2898cc7567ff9b1d5194b561e589d0756e781d5ff0481d58914e331e29f6050751e239ec788bfc72fe131e2798cf88788a9ce63c4f318f13c463c8f11ff2263c4b33b661e20beb2a5e81120be1258ded820b0bcd92fb0bcd52fb0bcdd37b07c87f8f0f5c8f272dfc8f2ca4691e5d50d22cb6b5b882caf6f1459dee81b59deec1759deda5e64f90aa3e161e56f9b590e0f48bf069007a4e701e94bbfec7e01e92b6c8847a3bf9f68f41933e351ed051ed59e47b5e751ed5b04c9e421ed7948fb0d2255671287c7d1e771f4791c7d1e479fc7d1e771f49f6c1cfd8ac6c383e8af10e53cfc3e0fbfcfc3eff3f0fb0f127ebfc292370a165ef0e8cdb150a6fd44320154d9fa36a2b4677c9ee727688789e727e0f909ee353f4123be0d51b1428ce74c284c8678ce84979033a114d2f7943061752683a68c0809f6b6c7d58ad409f2dd290b2a9907ca540c2748030ccef3d0834598db4fb9f756d6005e9479b31f235446dacc4b8e62701d84973fd39d35f3e2608a332aa493593d5142ae3c0e04329f091bf0774dfa8432b144d9e536f91c2a4926d62587a87ed0e72c7ef0ba14146522885a3a87dbebd4a13a6278154dafe00c86a9330930c3b875c6aea56baa293b9a69d98ee603cfb135603b63d5039ea6aba6e55a7d123c5ce2c40e3874fb9e629a9244679d9de316591fe68bf13470073b851691f51931b9088bd93778167eb8ef04108d8ddf432288f34f67fff7f0d3c5d7c1d9c5fbb7aa218c84b788628364229c02caf185f7683c07c3b70717c7ef8fcfde33950743c422bf9ca0c7d80b71543ce162115fc15b21f2111062830417dab409b28d6b1f7ffcf8e1fce2ecc3d1d7c14110df0ab2a54a83a1a19aa66ea83baa6d0c15dbde5115cd363463f8f1f8f4fde79fbf9c7ef8fc7580a91e0e86f2efbfff3e3cfff4e1e418358a5a80e175e04e0643697870fafee331adebc14bf40615ee48b2896849b1155db66d4d5511306efbf8fce2fd60a8210a41058767a7ff7dfce9e2c3d9e9d7c18a2f3f39fefcf31982fb8c84587205e3040a27307601e6240c42db9230c2f3834f0708e2f8d3d7c14784275d78108f4788e363239468455e06216ade6ed967a605cb92e45a0b5178b9b20951e9d584526d82300ec10798710a684aebad483bb66d1b92ac98b239bc383cc8dbc32ef5b778caaad8f52af6773831810091ecc27ebc438d70c5e6a9afe2319af084688d4c0822516e8dc9c4447578f1f51029d130094038547e1f1efced0322f337c73f0c86b873c3b34f47a81904da4c9579fd53549f7422035056027cb938787ffcf5e2f0ecfc18f505cd620821f6651612b28486486b9ae7c91a10ff9ee3f8ed3b03d4d0a7e383af031cc51e7ac22fa8d08b66c29b2fbffc2088028eca3141fa188215c6d122f4108fc384f1e597c30ba45ee2a11134fbdfffd2f4d7a7b88e21fffb5fa702e2a2022afc0d9728fffed7f15f05a4c61134d535fc06f512b3dca1f01ba28884c05db8518acb7ed8113e245342852720cc11e0febe7d7bf6b7af9abd63ea43d146a4373464fcafb24348b99c08c2af7fff7d555e9a1ff19fbd7486d69df0e314a48eb4a7d9e817da58487ba222fcf86d8f2544e1c71b5c83d080f0e32dae23670f703a9d277b00f39f1f318d267b33e1c730c2510492d5d96f1424ad5626c0217d5f9d04077fe04f599d3bd2e1ac67c25d92e590a1ea9c318774157554e6a973b6993ac7c5918adc2a5d1321be3b626539298fe24b1006df419648a6f25899f22809b23ac54ff6fd55849424ac7590bfec1b1fdce02c3de8df0a2dcc4030c57b1df2f7ffdfde9736b76d2c8b7ef7afc0e3adba719e4909fb9250aea7cd8a4e244b91e438b9ae146a080c253c930003805a722affe9fc86f3cb6ef7cc60254871f3921cb82c899ce9eed9a77b667a29e7c4116b32fb533989d61af5f546086a859c56c869859c56c8f95a859c2f2fe63c27e83c23ea2c2bec6c47dc595be0d940e4f9f431d59a981cde1931ab26c7321d4dd174db5435c5b0645bd10dd5d255cdd2144bd635c3b4706b96e1afa11ab6aec31c311cdb815dd0326d19715453b66c59566d43b32d53366cc7306c53532dd3b42d5bb1144397754cd50c45d76c553781bcaea98aa69ab6edc057c506605b8722e047d60ccd3214d3b14d194a5364c3320d43516d55b64c55377447961d59b31d4d05645db6a030db906d1d8034cd01da589aa66a2640a9aaeae8b6621ab66c1b86a341a9aa06ad852642d36dd554655dd1750b1a0f95d5a0b69662cb8ee2002fb56d453564a8bbac9a8aad2baa6e035dd3ee487e30a621bb9566269863f2f88857796d67aed399d07b7f7035a218bab358e6ec5ef3a99da3eb766b10f26e85fe7d6a67e79add587d5bab87d8cb7806c81e913f63a23e37a69f389bccc4f513ee0267a8cdc6f66366ed95cdbd8d97d9c6cbacc6cba49971cd3361326f51b3655ea44c6551a4cc4494d086c76cc363aeeac8b50d8fd97acaff3ce1310d9f8080a0d83d6d689b3d1da487dec0548d1e916d591fcaa66ff8761b1eb30d8fd986c7fc0ac363ea4ad791bb8aa1b5d131dbe8986d74cc363a661b1db38d8ef9d78d01d146c76ca363c291441fc8a63c20566fe80c9c9eeec983de809061cf532cc7b32d475174e76f151d733331ae0d8eb985e0570abe1eeb5d456ea363ce8dd8d446c7fc2b33d7363a661b1d3367b1b6671079a828bd812a2b3d5d35ed9eede9c39eec13dd334c4a64d9ff0f8c8eb919236e8363b6f2c15f583e68a363b6d1315be1edaf1d1d3346359398c6bd7b55306f76981d3c71b12d4de39ae70826acc5244985ef135e8311729924335be526b9496192cbfbc61396ae7f509f57ace21c253783ad9adff3e427093d86801c325415c5199ab46752d5eae986e9f46c9dca3dc7544d8d98ba43b4e19f588990396749e329cdeac86ac7cc78f73a6f483c960ea609f4789248179e379dc09c7fca07766e3e6f49d2f47e5eaf2a218e62a943ab27ab04aaeaf8a4e768a6daf33c59a1a64e86c430e7565529aa1a5366cd5bd42d4f58a13243cb313dd924bd8121cb3d7d287b3d4787ba0d08313c7368a9aa33bfdfd4bc32679424d5da945256a88e252be6d093f59e6569204e527bd81b689ed27314473675d3d74dcb9b5b1dada84ee0e14c2b2a937d5f6598744573a8a6f74c4f819eb164b5472cd5e8394399289a3e20de409e5b153dafca39538a42e369691f6d1bf053b1173766ae50498f128f688edcf315c7e8e93ed4d4b6f0e55db77c984bda40f695b99534f24abe0b3f86d143d830e145cefc4af5779b56a838ef259fe40c28b777ac5fcf1dabe111d85b55bb3718e08ad534b537306db9476ccf324d4d1ba886fc77ba635535adab286657551c3cdfb597ac5fe210a5195dfeff2b3d42b517acad8cbeb28cbe450ea9b41cb2e5905f8c431a4657512df8693964cb215b0ed972c8af9043aa2d876c39e417d3d351803b5a725785fdb9e5902d876c3964cb21bf3a0ea9b51cb2e5905fec0ca92a5dc5811f456e55595b0ef917e0903e0d136ab42cf23f8945ea2d8b6c59e4173b44ca7657c583a4aab72cb265912d8b6c59e457c8228d9645b62cf28bb148d5eee63fed3debdf864586d1f66c1dccf69eb5e5906be8d3cf28b8f6b95bc8de2764a6ade26bcb4cbfa0e22b3051cdc49ddc505a56da9e365b6eda72d3ad71d366ded98fa394d9a5950d4ed0a491add9ddb253e6dc5cad08ac47452b85e3dca35957c739c821ae332908a18702b657ef3641b56e83fe2338fbc031f5e1d01ef664cd377bfad0327b36d18c9e6eaad4571d4b912dfbefc4d991062aedb6b7c82d5f6ff97acbd7b7c9d79b596729a31a181b634b094b53b63d5799758af1b71209d6f508a6d74d331cf3d2ff988ae00508faf0433ae656dec3284c7fa471c89cc5c3b63dc6c8c2384d7fa0c1ed9de0cee874ff2defddaa817b989ea6201f789c53246507fc823676f7470a75632922d4f6c1c7dbd8670206b78d28fd70ac37641c6008e6f36be9fa8e8e46d2d1e8565279de7b5131232b01841f9888e8e65f4484ae6f6c4cbaf04b35656480a75c4f88c71a9e031d44233f0779871214f6054b9926f47a3a4830345cca03f0c8591c693ea7f63afba42301030cfd3c684156411e84e0927b89a8f5b1c8a3507e98925b5a6071b107bf9d5174165aaeaf472601b6e88f3c98015b003e8b94bdd7b9a2b7d31141bf1d30f2a2ab0db9cbfff34d15322e9060fac43d56e0f738c03a708a85cf9dbab835c4e1092a71108b5936980e872053f03f59db13bec7f1448cc5cd2accbfbe8d50e4e0f10e58c241a50b0b42259072c579ca3fa06f45db15d52ea32d9e1c1c2e9f8cd839d98fd1696ce098241f25fc552f12d39e2f10a1f842950b94fc4b3910449659ea404cca7c42b0a00f096ea9985aea917aad07c483058781e5a4e48e4ce80113a7de231be28e2930110bfab5f88a43326f85e6f0bf14f09ca6c028bf32141857e2e492379ea7123f087ea9d663710772b09f4fa08a8ca1b0efe509c11398785222cc132a0b312fb0a14abfd63197a854039925b038f95feb7dc9c66709f4d22c34f5529b665b9aaf3b5e4a65955546a8b12ab3f45803cbc9b38188b674161c977cb4ace4ef6e35e96659f9667309675519677b52ce7c3967694967257f78cb9e0ad7effdd53dde6dd5ebddf3b7c46a5751b42e7ac05bee34f369dcdf6df3a4facc12f082d81badd7776b9d7a3ff9c9778ba7df4f7402def629789bfa549fcb3bdef68fdcdbf792f78ca73c756d129fb017d7f498b77daf798bb9d76a9c7e796eb30d6ebf3abfdf26c77fa6d756e0fab3771cf3f4e5b62aff0d7307cb2bbc0ab4b2df6794fd1a5e0d3ebde4f749de059671e651390e7f61c9ab95981a741bb72b342da55df73925a64a278a23f93a8c7d4dd1b0e5c17f151edcdf2d2eff6a39c91df1a30789ffc1ab9c296cddca0ebfad83247197c7977c768b07e94bde860164f9be09bebee31e37e52aa193513420a3ecde2eaf0abf56c2fd81a7966eabcc2c4d14aec83592fb782100e95a51e7ec7650de319dea3fb38a5b2f97d76699d672fca32049671ab38ff2ce453862ae82ebb7b37eabb12056c082b7b5a4fcf2b25b0f519ebd6dcd3e7981300962a71403778d137a14c4e5b73736177dea81648a1e9935104c7dbf09e6212693c33b1263874e137a4e1e319cd6190d6fd3bb3751bc3f4da3f700c2c67d32225e6341f9abcf3ec665e5be79e9309d01fdef51fa3d9382a1e26fa7e301bbe347a15994c28be50ea4f1f96886c06bacc434495829b5f9d667d563ee755955d839acf41af178388defa98f8d65abe8021fee7a2a2ca4087a309e8647221e1e838ff225239e276a40c5629ac44114b31508a46e6904c7e5f8e9848614e4f8282e3f7a64fa4d6211e31b5b5ed143382d4cb207b052fd61a41913dcebbc89f075f3326ba4180efcf806a396f31303db3f79a32f790cd2135123e1fe77992d2eab276f23abed11458fc549873fb1e237e28b566118c1523fcdf440561b6844519761909e86a286d9eed13046a7210aa5ec7155f08fcb2809b06e1731db716faeba3767dd83abeec159f7aa7bd6bdb9868fd75733c3b5b8c19e08417ecaa2b7f2f107fc1199fc40427fc486e512175a985ef0f402e7fd5d2406f3f729f12f449871bda1237089f16995cf4a1127f6b236926a790a88d753dce0a1271e2f8a40e6319d50929667a418039fedd4fc513aa1d0a3c5eb87c2739f990015c28b619fe65688d7206bdaec7ae56a7ca804f0c7297c7ae44fd121c3f07f0e9260007b4acaa70606d73dc3f99b888374084d78bc0c1ee9287b4d96915746b097120fce6cc4633d8ecffe34be65310293fcd9fa2c1807690919191db6ae56284b3b0fc2c2673c8cc525897963b3a2384b64a084b7611c8422847c4e7e3ac199055c33cc5ac1e73bec825982ca1ac05a1684a5ca6945517cdee089131b50c2ad77ed227ef32cdbdd84e92ecf72b7c1709bd8ed0266db17ba24f3944ed6e91e7edd7759b9ed83554ddd68e80621f13c541ae1736ac5cecbb52d668f5758d833d8be7f5974fe7f808cd5148dfb26da1f8d70bd16e16787200ee5af3783281a3dd78fe29e6785236e2d3a35aed0fdc64902df30422ede0fa208b12ae13cea694e12c5ab866bc9fffe7d1aa5dfcbfc8fb8a0e45f149156bbaaac661697965542ac1afc332f34832f2e3105a10c83b58f7fc6daf34fafb1d6f34e8898575e901c05e67109b7dc2b3c193f8a2279cff02fb3b5105dcdbfeccea1565467311cd26d28b598d94b959f4d6ef67951b7942f2a39017e5b7996f72cbfb2ac0e5a904d877fda037d38d065b337b494614ff70cbf3770ec41cfb655c3332d553107ca9f022bbbecac4e0c71ed592960cd1123786ea8c48e6ee84979cd01f4c8841da11a48f297f075e9b2a0d458e906cac6f7eac6646b37960da55485b1cd0bc4c21a8a393f5f9374351a381314b738b00dd43f53973595bcf5be030e062204ca8dae38276cb1eff26bfa069aecc27e4dbac5e575036153eef2ffaa616c427fee72c66bde4d08b32be8a65eded13626bbf509b2f5f5f45997d0275a3508e10e8311accc2df60c808c5d5f9c6b5d90cb3f25edcf3400b3e56e7b2caa25c09f38fd64fdc6a87f919ee3256fbdef1e28f9e856f94c081b6b1cd238d9622f4e13d8ae9900e0ce9164d6a5dcf474bd9d1129cbc8edc961e593837864631fc5c3da8a67cf7110f2abfad963b11f4de1a8b03481f2dde2ba2ff27348bf5bf135bee949ff4d1c8df9e17ddd96d6e96cbfc1f512b6d56e76b7b879b319994fd96a56c06a8d6ebe3714b783558b366e855698b265866b790adf3c9b2e1c175f25569bf3d31b3444bb7e0abd5d22aec14463a0a128432e6cd17c5a694abc3bbc7e77210ff132b2fffc733d8a1e7491ef6eb38ed93b89cbef135c9a5bda2d770f3997f0e42e4aa36db53ba623f68292dc0513606a8fc1783a76eff1bda034a3d7259ec6c4fb88826b2e54c4f4f76910b3871317eacf4a637ab8c555a636ff3af8f982681c47218da649498e21437a3b25b19f37af56a0b24179634a92a9684fe5e2779346a4c1b8da534083c640d84da81785fe667dc55099fc3298a629085f8cfa904057a57730432b017097a5ef4fc908cd447785b96861569adfd29fc180cc7994c8ee79f9613748a99b1d7717cfbba6edae7acb3d1e50dfa7fefbc0bfa569b2eb45d3d255f5b28dbb2771801785684f9b1438cccdc632783f63718b10eb0d813d7bceeedbcf5f4f5ecbfddde24b963de4ef730765a899b417659d50a167f35a114a4ad9f70ce83ac007eb4311b2f72820b731195f094f2592cfbff307bc1f608423fcca636b0730bfe819b3b6c545566aaea09211156f90d98bea39d3d4dfebfcc26d6df99b7161f2c78c52730d29a1f2f45f43f6af234565a3c2ccf6d6c83f2f52092a208402040df7b9d63353a52aca2ca726957a014ac94613b2e69585c9477482dd187a50dbfd98928e74971970332da8e861ff3148c4d336ebb08aadefbb49a1d190bd55ab965c52e1958bae5b548f0392ccbc4bc3de7ccdeb1846f0993ff3a35a0d6f1d561076d3591085be926dacaf5070e1e6c024b35c355817656326cbfc715b3082ec51b9b2a0f0ddb878f19312669b0efb5434bd15bdcced28f185bd6cfaedd3c48b8309af43d526bd6bef68dd9ed235d0aebad41941616d3e1036e5d5c59d4f6a6e5e8f857ab5a6b0ef65c356119bba4e0906f6ba49f7ef537be261e24ceb4be213f89268d6b13f63e263d533cfd0d7a86eea66cf317cb9a76bd6a067ab3eedf9b2ad129d98c4719c3f97d7c45f674c66dfa556d4759e31c55cdb12b346297fb3c96919dfab1b11daa27efcdc22d6d190af0950b32f2f1b0dc9f36f48dbeb8879af461bf6c8ec3bd1463db28901c7ac8ac2a6a69f0d14d756ed9f25b596eba3f994b6309a5b98d29f78166f6de2969e6a366aefcc43c676a97db28e6c7e80d9ac4f1b9e26b6d81b4d8f2c9faa3f8a67950d7b64d143ca467d537b3ad988d696fdffcd37856c8d46b76d34da68b4d4186663b7e938d3dfad9df28bcb8d85570a399848672e93ae33af69258d6a644f750573c52e2989b313b4f0525518335475b795b2be7359baeecf51b57d66eeac3f67969d2b9bcf91d9b9d1df6db869da6d1a803c37bbdde6a492dc2ae0120efe41ee4e2ea6e3e89e1e4d2770b086e17e1bf95c5bbd54345eb9dfd3c33bea7d2c5b09f53d4c398cc261703b8dc9f20f0eb749668d7182164040a4b3e468311310ea035a72c03c8c2dfb483497cc714dc9f6592dddb994d8202cf96ed030bcb39d590c736d248bdb41767397359cbb1deedd53b43fe06b8b3d5065f77ba5bb927e4ca1f328303b9ffb3b2b72d8bd49633db24cc92be78a65fd36c20fd9ddc62dc8c59506533f48f90dafa82d5a171d43627db839ed390f70336f69b3d0fddda2a872f7b29aafd11271d5fd3769cd84c41e1db9d3e9dfa64565d7997fab16e11b4fcdf1da5f781581989a3e3740470072138ce9d7de18fad537467c6de626641490a4a22cc052b20b73dcbaa52017e1c45df5ee3cf06c7fcc5094e751ca9b5086a63e8f5671922bd0b4a5d1c472ca10f5255ac68739c3309ec7a0550c731603e4f06aeff71390bbd2cb0884afaa02c704939e2a83c293606a65b25aa52655847c585641aa0ccc2a8895a15903311b9c955a283a7b151cfa3c0e9c9e9a46a42fa4abf21089a4ca18d192afe68e442693d1d345f86ee2b3379eca63551d3d1fb1f54954c66f7d3295d1dc984c36b61bf48c18b5f529d01529c001bc36dcb86f27694c82b032058ad4ca2c9886c1ef53bc478a855701dc15a3349c8e46d5c41255b69941054b00d536cd96954f9999f2e4a6f2e45a79f2aae555e6d7672ab332193f6f99d9ccfd4ca566d3fc3315473f59714c6ca92f96520d4a8a47cf2f255412604ecf1e2735d6bb603dac825499d4ab205666e61a88d9f45aa98562d056c1a18b71caa3d53032fd62cf64f44a3705b9ae0557c96c608dfb3c2cd2470abcf69084f7a4b8f5fea72cfef51a7e65fffeac484e738a2bea91e2550e1799a5248ad372e004a1ee5e8c59a6bc8680c2a3076a7130da5cbc16eebffd389a1c450f61a7d2cba3e938ac59f8b334e92ef07d1a966467368d85524e4f293c2b4267d66e85e651c8e7f546542a137d234a9599bf0d4ad952d8ac97c43c17444c671d227403224a4303c45cad6a12d6664f696a97667079e3f499df193262cfcf49256cd9034fdbad9f636bc96c953793e92769843e6e8ab55f60e159137d26496934a231e1d7f1aff91914d36700833048771b533dbc5866d1d750e1714e4e13e23018d10949ef5e57b1f2e47985bdeeff9f0f8747fb37fb1ffe4beafddf1ef02f3f086fbf93a6e9b06763ca8b4ea7f3e2a793d36be6160ac3bdc058927b2a11e9f229bd838d6b380df90e96de91540a784418ea4b0f7714d3284384f417d18486d4df79f1e25d422103200bd408fd5fc1ae0b5bac348a6e030f939ea0bdbc58403a46ed5e460f27a1140d39ed8c42c0cbea884a9d42fbf2ccce0b36da3b2ff6432882a03e1390c54be9e43bd6be611c8da5df6f8364e7f2e9a774e72771759048c178023b9ff413fffee2056cd9d2f8c9c53ab9d89c977e40a0ba5deed7a92b0915d56fbf63fd8d37d2d25e96b8935d50bffc96e5c25c4be36804009cc8ce108ec48777c1c87f29caeb4a9df327d4bc62b757dfbef8edb7d7b393a2a232cb3a6b3a6193345399ada495e7411443add1a0e2967b40a23effcecbc873cb28b8f2662e83aa9b4206568fdb5786e5a7c02500ab87b5251092156a511c269700ae1fd496471147c42510983eb54bd239d07c5cca43c0c3345d8437d1e499512900eb42efccb82c075a1999e5509295ea928fce72e0b5f15909898fd07228c518cd811726489541e9c7749ad03392a44c857e8674357b96d3ee2e8f715bd3c87f06bc3c8c2ba0252bd72b1bce1550aa43ba3a221bd615d0f2a1ad84326d1a38a6a571c4350f98414aa1d95de03e085b8d3c61c6a4e475de2bfd597b931c6d4c2637c1a4ac460b5c80277218f62dd771606fe0ff9f7a294b613b3e57e8650eab0506d7f00df2935de27a96adfa9ee2bb9a66fbae6e52ea3a03cb70ada14241ee91bda19f6f4525f4fb07f7e92175793bc2fb6874cf2d7deed084227e72079e6de89aa5b8ba653bae3e24beebe8c471079a4f7cddd02cdbb31bc85e5c9fbbd729097d3477d25415a818b64b744575758758aee310dba55477e800eae5db4e038d9328ba1d51f787a7410cc3aad81a350c40356d0aeda38381ebf80e812a79d4d74cd3d3d4a68a60c31234790331c0b588ea516d28bb03cbb15c9de8d425aa02f51ac8436da811a29883061a623ae64434aa0f7ddbf53daabbba2eeb2e21baef5259b35462f896e7a90d44c442cdfad51e78aa6cf9866b6804a86836716d19fad5324ddb747ce82f55e35484fa4b3efa330a21fdc368f2c4dc79b287f07cde8947ff9269114a142235876238b5677c989f2552fddda602fa47c12d48da7f944d35f7ef998f47346ae3ea1f09da5a558933ebabdd0528681954aeb3d2df6d06ca69c0461d3199938c50d02aa21e66f8b30059cbea8de89f4c839c2e3fcc33239103d83cd0d15999b06a18d035b3304df82731a5e1730472a0260a57d47f0e5f801416539578cccc58a891c41cb839749aba42ae1399e98b6af6fcee98033787cebc4e6984cac63c1fe23ed719292985a0aaecc1d339999455635e336d197c6dccb333526502fd73e2c55121a14fd841e6b098cda5d555e46594cac8fd736ecc59ace4d1289824b006ea948e2faf4fbeb36419e6780e93532c13c948e2de5eacbd9812e6d4b44e9678b0e6b2cc921e5cc90f6a1d859bb2e2822a01d5aa52140ed5896f8382455ea225f14f5382665fb57955ce2ae08593da1b61a95a921196daf5008e3f88bc41eb56b47cabed18b5dc429e985772ff3a24ccbf707d5988e4a22b8a86f72ff7cfca4bdd0fb0d0040f8c97343e3cafea7abde68bbd0ed44040b8fd6da2a1ee54a894218b71ce5cc19ec36137f092a6b55087a9205fc1da439fc3f310b3fc0ad23be197761e52965fcc814c79f238841ea53fc3f42b74f3b2716c062aed26c095efb89de7cc7e566415f077d1030ce6fe68c41dc33655b60e53472e7a7f0176015447c79d2c00d8860aa4f1b4a050852b78257d4cdf70c7e6d5f61619396cd6e56c13ad2f780c6122bcfbc372294366533d9fddfd4b92de1555d81f24d1088d1767eb9e65e5240ac47ccde78a9d05c1691a41bdf1ce69966296574c36eeb49dcd8ca4c23960a295f3f24a3417dc2f4cd9cb97f4c21dc45110270d824f6903624f04af8f0e4fcffbdc14be72535a2293d5a35e5e9f6b2a9fe34962449b26530540205d4f08ce8b2b3abc7e2ac6e4921f3950b46ad8407121cd0064d56a2008b30cefddc8e8905f918d5818d0922e0fecd27ea63b3433b164d5ecc90afcbf91e5efd8ff1d5996ff0737f11c2927f496dc07b74cf5a7416eac66162b092d1fe615af38bad15c7c052d6bfde296f6df1f5e73d5d3f94230c2bc8b1b44709e9e01bd5982d09beb9bb3fcd02826fb88a674a1047e1a26345e2ca4736eb8a0e4dd7ad15897e646bd2937eafc7a9f39703e2c9e52eb286fa310667213648988cf157c4fe2683a49b850761293c95d6d4b10cb62314685ecfb8fb94ffdf9944a4005324e08e2a5e72468ea854afe0cd2457c4b42119d7b1172196e860830f564313a87984504d1942ec443805934b14d2ec4143005f2117f4315fe8b12a8d2f9b826b3288c4213604146b8833f0d87d1bb84ee672f4dc8d072938de6f15b02b35c0c9d99a0f99353224dc38f61f4100abab484783a26b7b441c07564065cce2e907ea44f0f70ec9f7386ce5727671e1526c28896d00b9ae7e4713f1d9144343ba9ed980d10052ec86c20435d8967acc671ae8214a839efac63d90cadce5a21e92a8a52f473d2584c9659805fd35be497c11fb434a0cf2cdd853865d2f17de051904dd0314eda58a11acc0cf2219910e6d522a0c9dcdacc40ce90b909d2115d54010650a021dfe726243569af9257c0376fd9e7d52dfb1d5ea8c2de797a34af2925881cede6fa240efc82331d5e5d2fe4395cad7721cfa9d26465fc63426f6b2c906df10b8b62080b2184f4d3d45c9155ae54bd12ac665fbc5633353a17ae55ca53c30051878367b92584cbf0cbf76dad0eac5e7366ed4d69dafeced5052ab27a4cc9c590dd2ad2a4615557f3e7a01dc6c9739887a5fe1a108c0035399f4e267526875e948adc3ac6cd1d6dda0acbb93328b0bed1934d65879755bdc012003388005f2f2a619692ace70b7c0029f437a0cd29f3e053dc9e659b4d25af7882c9af5ecfa2dba4619e60031b808aeb3a743783e7bbcb6972572bb292d78c712a7c99c144af7041ad8e5e062c9b1d0e69887e9a0ee81d1c33a2b8c64967011a704f937d667558abfd2c4003ee9c1eab0134205edf45537cf162e1968a0bf0b97598035ff5dec595e02e738bbb7ac522261888b78c3a70e5a90ce7b26875d389a7f3fa9f9d1bf620d3f94eea74fee44f6933482592ccd71e93b098f327eeccabd6da39503915e180111f42f0e66a3f0a6a046601b25dabbaf5d42d5ccb2f91c8c3697c597fde996be9bb8e8deff3d6bd9bd8f5964d3eb97df782a6f5ef73575f3d16472b3fd2cd64487724e9d1c709097deaf70210d34a363862363018665f0a20e81f6d920356a76ae6a94ba86f764aef84ba665a06b13cd7f407beabfbaae7dabaacba03a2abbe464c47552ce80dee9532d30f6695aaaa10e455e5b6abbd1095bf562d6777896adf3ff0a7db614c29beb3bba328fa389db8549735c7b188eb2b0614e0d8aa4bd481ea52dbf286be65fa0a06af6c6e88b25443362a7999a6f9d1d885dd813e9011d304485c8d180ef495ef0e14d970759998ee40b50c57f31462a8962d2be8f26d9346ad59e6b2cd19932074ef40bc4f499ab88aac7b03dd945d9f52d9d5078ae10e7ccf73f5a1ac3a44373d8f0e366fce1a652edb9c7ce84bfa1a5084ed69b2a1d9ae6edbc4d555d580b9ade8ae49359baaba3554a9b779b336287b99e68d28482b65fa842a96667ba66b5a3a749caac13ca054738935d0fde150553c59dfac55eb15b9ec580dd05f24899f44470d0d876a9aa9baf00756a93ab05d9bc88a4b86b6a95047f12d670b83b44ea1cb34a83ef854b3ac8132545d98cabaab2b8ae63ad690b8f6c022b20edb111d3a9bb566ad12971d9b9991175bcf70a07bc3a1e7aa36fcd20743dd7514cf720dc5d32daa53d5b1accdc76893c2976d2063141e77b302ccdff506323555458139206bd07bc3a1ebd8bee9aab03d1943cf31894e366fd95aa5aec2748bae13bc4fd114e2c3ce0a1c5d814e334cd32506d160d9ca86e399b666daca76b8ee7a452f3b5e694cc20464f36cddfadad0a15487596f50641514d7ade50e60412bbaad7bb0aa371fae750a2d6857053f46b9eec3a592cb8afd39a00f49c548683e95a2f6cd453464b332ced8f8546d23e6c1d6eab3882aa44afbfe38089703e795d8bd18f9d2afef6fb8b67b22bd8473ae541cd9e08cf6f46d455d796e93fbbb33323a97f99b65fa3e1fdc281ee343007dccae7492d83bc2cb17e660e60dfc46dfc6c266348a7d74e64b2f267b9d57a812b9370926181a804aaf92944ea45741782ff19c744c634f7a3522a92befe90e7c8a42f8d453a5571ff7e41d0cd52e2baaa558d2ab4784607657d2ab278451c4173a1a4d923d12c44f197d46faee16af18ef82612abd621ff7a61fdd28712fae6fde82f4f4f6e65e85cf270769747c7375bd9306c30afa9469f987f7f8c4f6eaf1c90dc2bd98f8ec63344df77c7a5b9c4ea0374a6399f07755189ee42981a30e9e6df903fd5ee7fdc7aa338cfec3c7f4f5e5d5c53f0eafae3f74b03a9a29ed4a07319cd5933be92de12631125e9776ba07fbd7c727c7172725e04ef768ffe6dd397c8dfd10f577a46b0c14fd84661a27787dc368812829290e421f9f9d9d5e5e5f9c1e7de8ec638f29b62677ba267045c3d47634c7ecaa8eb3a3a9ba63ea66f7ecf8edc9cd0fefde9ede7ce8e0e53aed7495df7efbad7b79757a7e0c853235af87c0bbeb74e5eefedb93b3630eebb3a0d890b8232b966e68aaa31a8ae3e89a06c85836ea3f75baba6a59907078f1f6e7e3abebd38bb71f3a735a7e7e7cf3c305e0dde0746461dda5739839e832ba5322e8d83212bcdcbfda078ce3ab0fb0de3082bccfcc56427c2b0092c0556e61fd757567c93a974ab0e1e0502b210a6fe716d153d72a42ad16c16f7b872c72b304435a2fa5bc50bad787fb597938879f70c8aad48d2af53778672b015fc23733e815b6ac9a87be4ac76ca21346717ac7088905ba0c250b27d5e1f5874312c3293b206157fdadbbffcb294cf397c7df76ba58b9eec5d5111403a8cdb332837f0bf0ac1202419d8bf0ee7affe4f8c3f5e1c5e531d485e919f1a8db095b425d298d26e88d9e3d478fb93ad84e070aba3adeffd0c1d74cea4b3f42227041e9e5bb1fbf957a2c8cf95d1453b4a2caa4689c18ef7e3cbc961e02ec1a4977fefd2fddf8e62dc298cabffff55642a60089ef3145fdf7bf8ebf97309e0692a9aee19750cb110077a5f73023128677ed4529a67dbb239d2623360bcf499811c0fa1e1c5cfcf24177762ca3db7360ea754d057fab3b6c2a170301ab51967ffbadbf8b9b527993c20d517ffd6976ec290bce3e965e8511ba144cb816bd5e293f8993c07fadea9ad5dfe59fabb9988975c7dc5a26994297fb5cdf52c088a44a54f0c2d7fbebc59b30fa5e2860eb9dc4efd85023347c1abf665d955b0594322aae7932f5cf2cb3a41a3a935746bca5d9dc1c8ec86df6f2524b2d47ec29b3a5923f9182776137a49b30b29c37bd3fb9966c1db8cddbebe3f38333585eefa3184498130a9208ea7b5d3f01ab1d034fb2758986091de35b0aecf2e707b8932c007ec93840907e0bf37a19f0134b939786b52d6d695845319627ac9896be3cb065aa4b03ab8ae6207089af679d0f4cdd56340b18babda31a96aa6ac0e2e7ed85d948ed1f1ebebbda3ffcf503ec0e9b327ad8d08b193c2ab6f4dbac29a38c39bf04365ddbb6619f5fb69c3ad59c21bf04de5ce51eeaf2542bdce10760b57fa02acc0804dcf1240a6998e24eab1d49091b8f822fb0e1cab7dd9e230323b4e52efc853f55f14753cde7b65b6c0c7493f4ca079e3fde83b185617f7eafd474439dbb5762b9cf6c951ce4999d924fb4557644d19a75f7447ce15b654fe49aa42b6f89c536c8b6c7a3e2ebb68e3d0f745066a372c64565ce3405bfcc39a518f53a950d4f38dbe5e4db3f0df1e9058cf832a1533fea15027fe51cd4f29a96d7cceeaa9543e56534998e482cfd1c24a8aa9770fdcc9969951d321781371e3b513b63f963a7fcc94f9df2360e9db563e1364e849b9e054565a497bfac7626cc6a20bdfc75bdc3e17b3ac80e80eccc755f9e1935d62f0d68fa003358b28d1dd9fcf7bfae1986f8f2b6100c580a970df8c7ba78a0d986f59c78c0b770b227d69ff46a507cc48d1d8e56f399cbc73da57cfe223e6722ff0f9d6b49afa058fa982e236b403de7ca1a59e6025983832c256bcc328355a48f4d8e63ab8a1e5fc371acbd576cef15db7bc5f65eb1bd576cef153ffbbde29c43747fb7e945316be4394d092a1f0ac4c0472b8f6140e3d7fdddd2179e3b2131a69461669232671ee1ed94dca2ad4dfe91e7e09335a4a6b946743f658618905432c8209975080c72d550c4e3e65179b351b111a0425252eb8ecac65efddd68c6f68b398216f655d0868aa9152a554681c7cc84d8df5c259a3c421afecedfb9c7689206a33bce4dd3980f3ed61cf6e745ee6bafa8747f148479dc947ed57299190f0a155215ce383d45e9c9f68da27fa76bdf697998ba43a626ee17baa4252a6ca144f1eb238cf00adbdd0f41e8dd8de8135f2eb9cd31d334c746371423f4d073edd486b9d2df0f4311c8963705cd17a2a9f00184dac9f8427f14791f13ed88a71d44d1c7318945d3fbd7344c22116824b36c409cdc082d0b94cbd424de2554c030165a8a48d3e709595708e3b79f9983218cb72a3d8e09faa1972d43b3951d07a427d9720cd9b6754506c9bd233d710045b1e1df8e65db8a291b8ea56b8e6d189ade010a01d4a2a7eb9a0e9bafa699b263a8baa5e1f6a8c906526000aa2a9bf28e025ba60aa77c157629280164b59273f76505ca569c6cc5c9569c6cc5c9af589cfcb2c2e42251728120b98c18b9b910b99608b9a600b992f8d8283ca2338f1ad3e47cbf812b97b935f3879db36b4183c7142c8753bf823908b2c81fb4022102cab3387f05b5537faf53b8f4f86e777737be78bcfce91d0fb49becf8834e213711b4ae1302c46e539dca95454f14857d3b7e49e984959f7dc175bbd7f14518f62b1e85bd2379d3f174c43875e6c0021a745bc487e81f8fe83de1565a3573a494c6e87f01d2ef4142ce2cd0603c4a26e9373518166e93073964c16d4bd6322022578179cbe7949f35fc2848d06362def6c3fcddec2633933ac4e8a345fafe6390080fe862c8b24e3fa004f741e11507d525073c65c6d0aa297c5f2d56347720e32614ce108c5fe77662cc3b65455bb309b3f01d6f764a2666f3d18298af3ce6d3983404a35d881ee3a68d61822b765ecbe162e83d77329a26cb85156c4006c97d9ac0422a779720856faeab50821934c276fc41e368ad0a35d565ced055031bf6772b1328f32398ef54c5142ccdb062232b5e7cd7986e04ad72eb232f9c181d31f12b5910a2f10b4fdd8de7de881286cf05cdf587be426c332a1b2d87643a1c068fffe9ab69fecac9dd9d66c9872c3874d9c07ec5b368fbfadfbefeb79a669b699a6da067b648cb6cbe8ed9121a66cbeb97ada95db6e603ef2a9a65734e357376bffcac50938ccb42f3c924c90566e8b168dff7991bd39f51bef6f8f52306504a913cb49a795258d1c3f80c7e26d6039d284961fa261598373c8c47c975033b26d4a95c337f52c09eb89ba3bd6ff00ea087278780c6dfb0d84bdf945ce17f23e194d9634761ce77ce41e00f84bb538999795dc620b4c44f3fd2273823b173dd37ca37128fdac0c32661192e41232bfe7d87ff11fdc1bf482f312ac8b71dd6a187d1781ca499e3acc6ee7c5bf2da9f74f2c7023cac91f1e44d117c2a1fd0d2a8bd7ed1dfc52827af5ffc2f504b0304140002000800a762365a971effb6e50300000070010010000000724f785051555f7374796c65732e6462edd9cf6fdb6418c0717bcde2fc6cd275ad5585aaae386411112584c37a9846185655d175aca4824d42919bb85dc089d3c4a95271ca0dfe1824ee08893bff0b2740481c4696b74efcb6cbb24376b1be1fb9f1fbbeaedee775fb3eaedfbe5f3d3d687ab671ea765b96679495aca2aacaa786a128a3a2a2e8ca5466f41509d45565beacf2e1c54fe9ccbfcabaf652c93ccffcb1fc73269ffe6f7990de4dff9eaaa7fe49fe9afa38d94fbe97f82df13cfe57fcc7f85aec17ed65acaf00008077c55bd3f47c5e1de63debc4b17bdea563b76ccf6a589e2555d61f1d9995aa69542b9f1d988674e95eb361ec1f56cd3df3c8f8f268ff71e5e899f185f9acf8bd7d6954cd6faac6f1e1fed363b37861397d7bdc52d85d8fea8ff3aad26c37ec41efdc19bd81d4acbee78eeb35a9f75a49aaeaf5bb9abebdad0e57c4805b56d73bebbafdceb4b4260f75d23e6b9c6dab2586551cb41c31bef3554d2f14d4e1a60872d93a719d72c3b3ce5a5647aedd958349d7ee8d4eb540d0c32755e3f0f8e0a0e87f5be062e1bb3b9abeb5a50edf1f87743a3dd1c7a4b02a059a34cf8ce1d86776bbd1b1bcfa8bde0bab6307633556347d73531de6442c3fd4d5f98e1c696e20eb64d485ed79cdf6592f18c5ca8a286be328dea9e8c73faf4851fcd699513c7be08977d460886f339a9ecbf993a12efab83a65a500f539fdd75dc7ed76ad5627d8fdd7cb9abeb1a10ee3e20ec4f8c79f1979f46ff3eb0e76fc793aaaafaeaae77ebfa363f97a8f73a76ba19f143fe007d22cf5cfa9d7ceccb9bdfad9eae742f1d4ba70bbaf9608fee0775251fdd1e6cce4f5e776c92fa587d9b8a6efeca8c32762be5d9b99bd1b0d0979065ebfbc807ba824a2faf1ceac7bb811b156bad1941c4634f1e8dc1377154c03a91293ef2678690177b21b7bd3a3544ece92548dff705b3c4a2b621a4e326c5a8aca9372d2be807197a3517d6f7bd6b803e95e9a96b5c19278483e14f9eee7eca4109173de6f5ec0704b91a86e6ecd1aeef4e9519a146fbbaa787aec06d2537cde7a4d6a2e608c1fdc8aea0f37de9c987e5a2e8dfef6c75ebd00a495ab953e000000000008ad04eb7f0000000000428ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f0000000000c28ffd7f000000000058ff030000000000d6ff000000000080f53f000000000060fd0f000000000058ff030000000000d6ff000000000080f53f000000000010eb7f35f3a7323a0000000000402864d58892d32fec6eafe9b6cbe54f3eba9f18afffff56460700000000000893c4522e7ef54f80ff01504b01023f03140002000800c962365ae720d4a883f10000d5000a002a0000000000000000000000b681000000007b33643565363061332d633339622d346431372d616464372d6133636366633235346362377d2e716773504b01023f03140002000800a762365a971effb6e503000000700100100000000000000000000000b681cbf10000724f785051555f7374796c65732e6462504b0506000000000200020096000000def500000000');


--
-- TOC entry 6357 (class 0 OID 0)
-- Dependencies: 249
-- Name: layer_styles_id_seq; Type: SEQUENCE SET; Schema: qgis; Owner: -
--

SELECT pg_catalog.setval('qgis.layer_styles_id_seq', 14, true);


--
-- TOC entry 6023 (class 2606 OID 39597)
-- Name: dom_boundary_types d_boundary_types_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_boundary_types
    ADD CONSTRAINT d_boundary_types_pkey PRIMARY KEY (boundary_type_code);


--
-- TOC entry 6025 (class 2606 OID 39599)
-- Name: dom_main_habitats d_main_habitats_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_main_habitats
    ADD CONSTRAINT d_main_habitats_pkey PRIMARY KEY (main_habitat_code);


--
-- TOC entry 6027 (class 2606 OID 39601)
-- Name: dom_occupancy_types d_occupancy_type_code_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_occupancy_types
    ADD CONSTRAINT d_occupancy_type_code_pkey PRIMARY KEY (occupancy_type_code);


--
-- TOC entry 6029 (class 2606 OID 39603)
-- Name: dom_ownership_types d_ownership_types_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_ownership_types
    ADD CONSTRAINT d_ownership_types_pkey PRIMARY KEY (ownership_type_code);


--
-- TOC entry 6033 (class 2606 OID 39605)
-- Name: dom_site_categories d_site_categories_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_site_categories
    ADD CONSTRAINT d_site_categories_pkey PRIMARY KEY (site_category_code);


--
-- TOC entry 6035 (class 2606 OID 39607)
-- Name: dom_site_privacy dom_site_privacy_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_site_privacy
    ADD CONSTRAINT dom_site_privacy_pkey PRIMARY KEY (site_privacy_code);


--
-- TOC entry 6031 (class 2606 OID 39611)
-- Name: dom_renewal_types dom_tenancy_renewals_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_renewal_types
    ADD CONSTRAINT dom_tenancy_renewals_pkey PRIMARY KEY (renewal_type_code);


--
-- TOC entry 6037 (class 2606 OID 39613)
-- Name: dom_transfer_types dom_tenancy_transfers_pkey; Type: CONSTRAINT; Schema: domains; Owner: -
--

ALTER TABLE ONLY domains.dom_transfer_types
    ADD CONSTRAINT dom_tenancy_transfers_pkey PRIMARY KEY (transfer_type_code);


--
-- TOC entry 6045 (class 2606 OID 39615)
-- Name: parcel_history exclude_parcel_overlapping_times; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcel_history
    ADD CONSTRAINT exclude_parcel_overlapping_times EXCLUDE USING gist (parcel_uuid WITH =, daterange(valid_from, valid_to) WITH &&);


--
-- TOC entry 6358 (class 0 OID 0)
-- Dependencies: 6045
-- Name: CONSTRAINT exclude_parcel_overlapping_times ON parcel_history; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT exclude_parcel_overlapping_times ON land_admin.parcel_history IS 'Ensures that the same parcel cannot have multiple versions a the same time.';


--
-- TOC entry 6039 (class 2606 OID 39617)
-- Name: occupancy exclude_same_occupant_date_overlaps; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.occupancy
    ADD CONSTRAINT exclude_same_occupant_date_overlaps EXCLUDE USING gist (ownership_uuid WITH =, daterange(occupancy_date_start, occupancy_date_end) WITH &&);


--
-- TOC entry 6359 (class 0 OID 0)
-- Dependencies: 6039
-- Name: CONSTRAINT exclude_same_occupant_date_overlaps ON occupancy; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT exclude_same_occupant_date_overlaps ON land_admin.occupancy IS 'Makes sure that the same occupant cannot have duplicate leases during the same timeframe.';


--
-- TOC entry 6041 (class 2606 OID 39619)
-- Name: occupancy occupancy_pkey; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.occupancy
    ADD CONSTRAINT occupancy_pkey PRIMARY KEY (occupancy_uuid);


--
-- TOC entry 6043 (class 2606 OID 39621)
-- Name: ownership ownership_pkey; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.ownership
    ADD CONSTRAINT ownership_pkey PRIMARY KEY (ownership_uuid);


--
-- TOC entry 6047 (class 2606 OID 39623)
-- Name: parcel_history parcel_history_pkey; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcel_history
    ADD CONSTRAINT parcel_history_pkey PRIMARY KEY (parcel_history_uuid);


--
-- TOC entry 6049 (class 2606 OID 39625)
-- Name: parcels parcels_pkey; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcels
    ADD CONSTRAINT parcels_pkey PRIMARY KEY (parcel_uuid);


--
-- TOC entry 6053 (class 2606 OID 39627)
-- Name: sites sites_pkey; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.sites
    ADD CONSTRAINT sites_pkey PRIMARY KEY (site_id);


--
-- TOC entry 6051 (class 2606 OID 39629)
-- Name: parcels unique_site_parcels; Type: CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcels
    ADD CONSTRAINT unique_site_parcels UNIQUE (site_id, parcel_alias);


--
-- TOC entry 6360 (class 0 OID 0)
-- Dependencies: 6051
-- Name: CONSTRAINT unique_site_parcels ON parcels; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON CONSTRAINT unique_site_parcels ON land_admin.parcels IS 'Ensures that a site cannot have two parcels with the same name.';


--
-- TOC entry 6055 (class 2606 OID 39631)
-- Name: layer_styles layer_styles_pkey; Type: CONSTRAINT; Schema: qgis; Owner: -
--

ALTER TABLE ONLY qgis.layer_styles
    ADD CONSTRAINT layer_styles_pkey PRIMARY KEY (id);


--
-- TOC entry 6057 (class 2606 OID 39633)
-- Name: qgis_projects qgis_projects_pkey; Type: CONSTRAINT; Schema: qgis; Owner: -
--

ALTER TABLE ONLY qgis.qgis_projects
    ADD CONSTRAINT qgis_projects_pkey PRIMARY KEY (name);


--
-- TOC entry 6064 (class 2620 OID 39634)
-- Name: occupancy tr_01_update_modified; Type: TRIGGER; Schema: land_admin; Owner: -
--

CREATE TRIGGER tr_01_update_modified BEFORE UPDATE ON land_admin.occupancy FOR EACH ROW EXECUTE FUNCTION utils.trf_update_modified();


--
-- TOC entry 6361 (class 0 OID 0)
-- Dependencies: 6064
-- Name: TRIGGER tr_01_update_modified ON occupancy; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TRIGGER tr_01_update_modified ON land_admin.occupancy IS 'Logs when data is changed.';


--
-- TOC entry 6065 (class 2620 OID 39635)
-- Name: ownership tr_01_update_modified; Type: TRIGGER; Schema: land_admin; Owner: -
--

CREATE TRIGGER tr_01_update_modified BEFORE UPDATE ON land_admin.ownership FOR EACH ROW EXECUTE FUNCTION utils.trf_update_modified();


--
-- TOC entry 6362 (class 0 OID 0)
-- Dependencies: 6065
-- Name: TRIGGER tr_01_update_modified ON ownership; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TRIGGER tr_01_update_modified ON land_admin.ownership IS 'Logs when data is changed.';


--
-- TOC entry 6066 (class 2620 OID 39636)
-- Name: parcels tr_01_update_modified; Type: TRIGGER; Schema: land_admin; Owner: -
--

CREATE TRIGGER tr_01_update_modified BEFORE UPDATE ON land_admin.parcels FOR EACH ROW EXECUTE FUNCTION utils.trf_update_modified();


--
-- TOC entry 6363 (class 0 OID 0)
-- Dependencies: 6066
-- Name: TRIGGER tr_01_update_modified ON parcels; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TRIGGER tr_01_update_modified ON land_admin.parcels IS 'Logs when data is changed.';


--
-- TOC entry 6067 (class 2620 OID 39637)
-- Name: parcels tr_02_update_area_ha; Type: TRIGGER; Schema: land_admin; Owner: -
--

CREATE TRIGGER tr_02_update_area_ha BEFORE INSERT OR UPDATE OF geom ON land_admin.parcels FOR EACH ROW EXECUTE FUNCTION utils.trf_update_areaha();


--
-- TOC entry 6364 (class 0 OID 0)
-- Dependencies: 6067
-- Name: TRIGGER tr_02_update_area_ha ON parcels; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TRIGGER tr_02_update_area_ha ON land_admin.parcels IS 'Responds to changes in the geometry to keep the area up to date.';


--
-- TOC entry 6068 (class 2620 OID 39638)
-- Name: parcels tr_03_update_parcel_history; Type: TRIGGER; Schema: land_admin; Owner: -
--

CREATE TRIGGER tr_03_update_parcel_history BEFORE UPDATE OF valid_to ON land_admin.parcels FOR EACH ROW EXECUTE FUNCTION land_admin.trf_log_parcel_history();


--
-- TOC entry 6365 (class 0 OID 0)
-- Dependencies: 6068
-- Name: TRIGGER tr_03_update_parcel_history ON parcels; Type: COMMENT; Schema: land_admin; Owner: -
--

COMMENT ON TRIGGER tr_03_update_parcel_history ON land_admin.parcels IS 'Responds when the `valid_to` field is updated.';


--
-- TOC entry 6058 (class 2606 OID 39639)
-- Name: occupancy occupancy_ownership_uuid_fkey; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.occupancy
    ADD CONSTRAINT occupancy_ownership_uuid_fkey FOREIGN KEY (ownership_uuid) REFERENCES land_admin.ownership(ownership_uuid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 6059 (class 2606 OID 39644)
-- Name: ownership ownership_parcel_uuid_fkey; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.ownership
    ADD CONSTRAINT ownership_parcel_uuid_fkey FOREIGN KEY (parcel_uuid) REFERENCES land_admin.parcels(parcel_uuid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 6060 (class 2606 OID 39649)
-- Name: parcel_history parcel_history_parcel_uuid_fkey; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcel_history
    ADD CONSTRAINT parcel_history_parcel_uuid_fkey FOREIGN KEY (parcel_uuid) REFERENCES land_admin.parcels(parcel_uuid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 6061 (class 2606 OID 39654)
-- Name: parcel_history parcel_history_site_id_fkey; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcel_history
    ADD CONSTRAINT parcel_history_site_id_fkey FOREIGN KEY (site_id) REFERENCES land_admin.sites(site_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 6062 (class 2606 OID 39659)
-- Name: parcels parcels_site_id_fkey; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.parcels
    ADD CONSTRAINT parcels_site_id_fkey FOREIGN KEY (site_id) REFERENCES land_admin.sites(site_id) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- TOC entry 6063 (class 2606 OID 39664)
-- Name: sites sites_dom_site_privacy_fk; Type: FK CONSTRAINT; Schema: land_admin; Owner: -
--

ALTER TABLE ONLY land_admin.sites
    ADD CONSTRAINT sites_dom_site_privacy_fk FOREIGN KEY (site_privacy_code) REFERENCES domains.dom_site_privacy(site_privacy_code) ON UPDATE CASCADE;


-- Completed on 2025-01-22 12:23:24

--
-- PostgreSQL database dump complete
--

