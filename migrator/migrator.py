import argparse
import psycopg2

def parse_args():
    parser = argparse.ArgumentParser(description="Data migration script.")
    parser.add_argument("--host", required=True, help="Database host")
    parser.add_argument("--port", required=True, help="Database port")
    parser.add_argument("--dbname", required=True, help="Database name")
    parser.add_argument("--user", required=True, help="Database user")
    parser.add_argument("--password", required=True, help="Database password")
    return parser.parse_args()

def connect_to_db(args):
    try:
        conn = psycopg2.connect(
            host=args.host,
            port=args.port,
            dbname=args.dbname,
            user=args.user,
            password=args.password
        )
        return conn
    except Exception as e:
        print(f"Error connecting to the database: {e}")
        exit(1)

def migrate_data(conn):
    try:
        with conn.cursor() as cursor:
            # Пример запроса к первой таблице
            cursor.execute("SELECT id, name, created_at, updated_at, archived_at, status, status_updated_at, checked_at, app_overrides, deployment_overrides, links FROM deployments")
            deployments = cursor.fetchall()

            # Пример запроса ко второй таблице
            cursor.execute("SELECT id, deployment_id, action, exit_code, created_at, archived, duration, stdout, stderr, deployment_overrides, app_overrides FROM deployment_logs")
            deployment_logs = cursor.fetchall()

            for row in deployments:
                cursor.execute("insert into deployment (id, name, created_at) values (%s, %s, %s)", (row[0], row[1], row[2]))
                cursor.execute("insert into deployment_status (deployment_id, status, is_pending, checked_at, updated_at) values (%s, %s, %s, %s, %s)", (row[0], row[5], False, row[7], row[6]))

            for row in deployment_logs:
                cursor.execute("insert into deployment_action (id, deployment_id, created_at, error) values (%s, %s, %s, %s)", (row[0], row[1], row[4], str(row[3])))
                deployment_overrides = row[9]
                chart_version = None
                if (len(deployment_overrides) > 0
                    and len(deployment_overrides[0]) == 2
                    and deployment_overrides[0][0] == 'chart_version'
                    and 'tag' in deployment_overrides[0][1]
                    and 'contents' in deployment_overrides[0][1]
                    and deployment_overrides[0][1]['tag'] == 'ValueAdded'):
                    chart_version = deployment_overrides[0][1]['contents']
                cursor.execute("insert into deployment_helm_override (deployment_action_id, version) values (%s, %s)", (row[0], chart_version))
                app_overrides = row[10]
                for override in app_overrides:
                    if len(override) != 2 and 'tag' not in override[1] and'contents' not in override[1]:
                        continue
                    key = override[0]
                    tag = override[1]['tag']
                    value = override[1]['contents']
                    cursor.execute("insert into deployment_helm_values_override (deployment_action_id, action, key, value) values (%s, %s, %s, %s)", (row[0], tag, key, value))


            # Здесь можно выполнить вставку данных в другую таблицу или обновление
        conn.commit()


    except Exception as e:
        print(f"Error during data migration: {e}")
    finally:
        conn.close()

"""
SELECT
    d.id AS deployment_id,
    d.name AS deployment_name,
    da.id AS last_action_id,
    da.created_at AS action_created_at,
    da.error AS action_error,
    ds.status AS last_status,
    ds.is_pending,
    ds.checked_at,
    ds.updated_at,
    dho.version AS helm_version,
    dhvo.action AS helm_action,
    dhvo.key AS helm_key,
    dhvo.value AS helm_value
FROM
    deployment d
JOIN
    deployment_action da ON da.deployment_id = d.id
JOIN
    deployment_status ds ON ds.deployment_id = d.id
LEFT JOIN
    deployment_helm_override dho ON dho.deployment_action_id = da.id
LEFT JOIN
    deployment_helm_values_override dhvo ON dhvo.deployment_action_id = da.id
WHERE
    da.id = (
        SELECT
            MAX(da_inner.id)
        FROM
            deployment_action da_inner
        WHERE
            da_inner.deployment_id = d.id
    );
"""
if __name__ == "__main__":
    args = parse_args()
    conn = connect_to_db(args)
    migrate_data(conn)
