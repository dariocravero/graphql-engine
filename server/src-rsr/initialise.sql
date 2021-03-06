CREATE TABLE hdb_catalog.hdb_version (
    version TEXT NOT NULL,
    upgraded_on TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX hdb_version_one_row
ON hdb_catalog.hdb_version((version IS NOT NULL));

CREATE TABLE hdb_catalog.hdb_table
(
    table_schema TEXT,
    table_name TEXT,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name)
);

CREATE FUNCTION hdb_catalog.hdb_table_oid_check() RETURNS trigger AS
$function$
  BEGIN
    IF (EXISTS (SELECT 1 FROM information_schema.tables st WHERE st.table_schema = NEW.table_schema AND st.table_name = NEW.table_name)) THEN
      return NEW;
    ELSE
      RAISE foreign_key_violation using message = 'table_schema, table_name not in information_schema.tables';
      return NULL;
    END IF;
  END;
$function$
LANGUAGE plpgsql;

CREATE TRIGGER hdb_table_oid_check BEFORE INSERT OR UPDATE ON hdb_catalog.hdb_table
       FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.hdb_table_oid_check();

CREATE TABLE hdb_catalog.hdb_relationship
(
    table_schema TEXT,
    table_name TEXT,
    rel_name   TEXT,
    rel_type   TEXT CHECK (rel_type IN ('object', 'array')),
    rel_def    JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, rel_name),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
);

CREATE TABLE hdb_catalog.hdb_permission
(
    table_schema TEXT,
    table_name TEXT,
    role_name  TEXT,
    perm_type  TEXT CHECK(perm_type IN ('insert', 'select', 'update', 'delete')),
    perm_def   JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, role_name, perm_type),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
);

CREATE VIEW hdb_catalog.hdb_permission_agg AS
SELECT
    table_schema,
    table_name,
    role_name,
    json_object_agg(perm_type, perm_def) as permissions
FROM
    hdb_catalog.hdb_permission
GROUP BY
    table_schema, table_name, role_name;

CREATE TABLE hdb_catalog.hdb_query_template
(
    template_name TEXT PRIMARY KEY,
    template_defn JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false
);

CREATE VIEW hdb_catalog.hdb_foreign_key_constraint AS
SELECT
    q.table_schema :: text,
    q.table_name :: text,
    q.constraint_name :: text,
    min(q.constraint_oid) :: integer as constraint_oid,
    min(q.ref_table_table_schema) :: text as ref_table_table_schema,
    min(q.ref_table) :: text as ref_table,
    json_object_agg(ac.attname, afc.attname) as column_mapping,
    min(q.confupdtype) :: text as on_update,
    min(q.confdeltype) :: text as on_delete
FROM
    (SELECT
        ctn.nspname AS table_schema,
        ct.relname AS table_name,
        r.conrelid AS table_id,
        r.conname as constraint_name,
        r.oid as constraint_oid,
        cftn.nspname AS ref_table_table_schema,
        cft.relname as ref_table,
        r.confrelid as ref_table_id,
        r.confupdtype,
        r.confdeltype,
        UNNEST (r.conkey) AS column_id,
        UNNEST (r.confkey) AS ref_column_id
    FROM
        pg_catalog.pg_constraint r
        JOIN pg_catalog.pg_class ct
          ON r.conrelid = ct.oid
        JOIN pg_catalog.pg_namespace ctn
          ON ct.relnamespace = ctn.oid
        JOIN pg_catalog.pg_class cft
          ON r.confrelid = cft.oid
        JOIN pg_catalog.pg_namespace cftn
          ON cft.relnamespace = cftn.oid
    WHERE
        r.contype = 'f'
    ) q
    JOIN pg_catalog.pg_attribute ac
      ON q.column_id = ac.attnum
         AND q.table_id = ac.attrelid
    JOIN pg_catalog.pg_attribute afc
      ON q.ref_column_id = afc.attnum
         AND q.ref_table_id = afc.attrelid
GROUP BY q.table_schema, q.table_name, q.constraint_name;

CREATE VIEW hdb_catalog.hdb_check_constraint AS
SELECT
    n.nspname :: text AS table_schema,
    ct.relname :: text AS table_name,
    r.conname :: text as constraint_name,
    pg_catalog.pg_get_constraintdef(r.oid, true) as check
FROM
    pg_catalog.pg_constraint r
    JOIN pg_catalog.pg_class ct
      ON r.conrelid = ct.oid
    JOIN pg_catalog.pg_namespace n
      ON ct.relnamespace = n.oid
WHERE
    r.contype = 'c';

CREATE VIEW hdb_catalog.hdb_unique_constraint AS
SELECT
    tc.table_name,
    tc.constraint_schema AS table_schema,
    tc.constraint_name as constraint_name,
    json_agg(kcu.column_name) AS columns
FROM
    information_schema.table_constraints tc
    JOIN information_schema.key_column_usage AS kcu
    USING (constraint_schema, constraint_name)
WHERE
    constraint_type = 'UNIQUE'
GROUP BY
    tc.table_name, tc.constraint_schema, tc.constraint_name;

CREATE VIEW hdb_catalog.hdb_primary_key AS
SELECT
  tc.table_schema,
  tc.table_name,
  tc.constraint_name,
  json_agg(constraint_column_usage.column_name) AS columns
FROM
  (
    information_schema.table_constraints tc
    JOIN (
      SELECT
        x.tblschema AS table_schema,
        x.tblname AS table_name,
        x.colname AS column_name,
        x.cstrname AS constraint_name
      FROM
        (
          SELECT
            DISTINCT nr.nspname,
            r.relname,
            a.attname,
            c.conname
          FROM
            pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_depend d,
            pg_namespace nc,
            pg_constraint c
          WHERE
            (
              (nr.oid = r.relnamespace)
              AND (r.oid = a.attrelid)
              AND (d.refclassid = ('pg_class' :: regclass) :: oid)
              AND (d.refobjid = r.oid)
              AND (d.refobjsubid = a.attnum)
              AND (d.classid = ('pg_constraint' :: regclass) :: oid)
              AND (d.objid = c.oid)
              AND (c.connamespace = nc.oid)
              AND (c.contype = 'c' :: "char")
              AND (
                r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"])
              )
              AND (NOT a.attisdropped)
            )
          UNION ALL
          SELECT
            nr.nspname,
            r.relname,
            a.attname,
            c.conname
          FROM
            pg_namespace nr,
            pg_class r,
            pg_attribute a,
            pg_namespace nc,
            pg_constraint c
          WHERE
            (
              (nr.oid = r.relnamespace)
              AND (r.oid = a.attrelid)
              AND (nc.oid = c.connamespace)
              AND (
                r.oid = CASE
                  c.contype
                  WHEN 'f' :: "char" THEN c.confrelid
                  ELSE c.conrelid
                END
              )
              AND (
                a.attnum = ANY (
                  CASE
                    c.contype
                    WHEN 'f' :: "char" THEN c.confkey
                    ELSE c.conkey
                  END
                )
              )
              AND (NOT a.attisdropped)
              AND (
                c.contype = ANY (ARRAY ['p'::"char", 'u'::"char", 'f'::"char"])
              )
              AND (
                r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"])
              )
            )
        ) x(
          tblschema,
          tblname,
          colname,
          cstrname
        )
    ) constraint_column_usage ON (
      (
        (tc.constraint_name) :: text = (constraint_column_usage.constraint_name) :: text
        AND (tc.table_schema) :: text = (constraint_column_usage.table_schema) :: text
        AND (tc.table_name) :: text = (constraint_column_usage.table_name) :: text
      )
    )
  )
WHERE
  ((tc.constraint_type) :: text = 'PRIMARY KEY' :: text)
GROUP BY
  tc.table_schema,
  tc.table_name,
  tc.constraint_name;

CREATE FUNCTION hdb_catalog.inject_table_defaults(view_schema text, view_name text, tab_schema text, tab_name text) RETURNS void
LANGUAGE plpgsql AS $$
    DECLARE
        r RECORD;
    BEGIN
      FOR r IN SELECT column_name, column_default FROM information_schema.columns WHERE table_schema = tab_schema AND table_name = tab_name AND column_default IS NOT NULL LOOP
          EXECUTE format('ALTER VIEW %I.%I ALTER COLUMN %I SET DEFAULT %s;', view_schema, view_name, r.column_name, r.column_default);
      END LOOP;
    END;
$$;


CREATE TABLE hdb_catalog.event_triggers
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  name TEXT UNIQUE,
  type TEXT NOT NULL,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  configuration JSON,
  comment TEXT
);

CREATE TABLE hdb_catalog.event_log
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  schema_name TEXT NOT NULL,
  table_name TEXT NOT NULL,
  trigger_id TEXT NOT NULL,
  trigger_name TEXT NOT NULL,
  payload JSONB NOT NULL,
  delivered BOOLEAN NOT NULL DEFAULT FALSE,
  error BOOLEAN NOT NULL DEFAULT FALSE,
  tries INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMP DEFAULT NOW(),
  locked BOOLEAN NOT NULL DEFAULT FALSE,
  next_retry_at TIMESTAMP
);

CREATE INDEX ON hdb_catalog.event_log (trigger_id);

CREATE TABLE hdb_catalog.event_invocation_logs
(
  id TEXT DEFAULT gen_random_uuid() PRIMARY KEY,
  event_id TEXT,
  status INTEGER,
  request JSON,
  response JSON,
  created_at TIMESTAMP DEFAULT NOW(),

  FOREIGN KEY (event_id) REFERENCES hdb_catalog.event_log (id)
);

CREATE INDEX ON hdb_catalog.event_invocation_logs (event_id);

CREATE TABLE hdb_catalog.hdb_function
(
    function_schema TEXT,
    function_name TEXT,
    is_system_defined boolean default false,

    PRIMARY KEY (function_schema, function_name)
);

CREATE VIEW hdb_catalog.hdb_function_agg AS
(
SELECT
  p.proname::text AS function_name,
  pn.nspname::text AS function_schema,

  CASE
    WHEN (p.provariadic = (0) :: oid) THEN false
    ELSE true
  END AS has_variadic,

  CASE
    WHEN (
      (p.provolatile) :: text = ('i' :: character(1)) :: text
    ) THEN 'IMMUTABLE' :: text
    WHEN (
      (p.provolatile) :: text = ('s' :: character(1)) :: text
    ) THEN 'STABLE' :: text
    WHEN (
      (p.provolatile) :: text = ('v' :: character(1)) :: text
    ) THEN 'VOLATILE' :: text
    ELSE NULL :: text
  END AS function_type,

  pg_get_functiondef(p.oid) AS function_definition,

  rtn.nspname::text AS return_type_schema,
  rt.typname::text AS return_type_name,

  CASE
    WHEN ((rt.typtype) :: text = ('b' :: character(1)) :: text) THEN 'BASE' :: text
    WHEN ((rt.typtype) :: text = ('c' :: character(1)) :: text) THEN 'COMPOSITE' :: text
    WHEN ((rt.typtype) :: text = ('d' :: character(1)) :: text) THEN 'DOMAIN' :: text
    WHEN ((rt.typtype) :: text = ('e' :: character(1)) :: text) THEN 'ENUM' :: text
    WHEN ((rt.typtype) :: text = ('r' :: character(1)) :: text) THEN 'RANGE' :: text
    WHEN ((rt.typtype) :: text = ('p' :: character(1)) :: text) THEN 'PSUEDO' :: text
    ELSE NULL :: text
  END AS return_type_type,
  p.proretset AS returns_set,
  ( SELECT
      COALESCE(json_agg(pt.typname), '[]')
    FROM
      (
        unnest(
          COALESCE(p.proallargtypes, (p.proargtypes) :: oid [])
        ) WITH ORDINALITY pat(oid, ordinality)
        LEFT JOIN pg_type pt ON ((pt.oid = pat.oid))
      )
   ) AS input_arg_types,
  to_json(COALESCE(p.proargnames, ARRAY [] :: text [])) AS input_arg_names
FROM
  pg_proc p
  JOIN pg_namespace pn ON (pn.oid = p.pronamespace)
  JOIN pg_type rt ON (rt.oid = p.prorettype)
  JOIN pg_namespace rtn ON (rtn.oid = rt.typnamespace)
WHERE
  pn.nspname :: text NOT LIKE 'pg_%'
  AND pn.nspname :: text NOT IN ('information_schema', 'hdb_catalog', 'hdb_views')
  AND (NOT EXISTS (
          SELECT
            1
          FROM
            pg_aggregate
          WHERE
            ((pg_aggregate.aggfnoid) :: oid = p.oid)
        )
    )
);

CREATE TABLE hdb_catalog.remote_schemas (
  id BIGSERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  definition JSON,
  comment TEXT
);
