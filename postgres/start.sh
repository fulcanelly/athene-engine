
export PGPASSWORD=${POSTGRES_PASSWORD}

check() {
    pg_isready --port=5432 --host='postgres' 
}

until [[ "$(check)" =~ "accepting" ]]
do 
    echo "Waiting for psql"    
done

psql -h postgres -p 5432 -d postgres -U postgres -c 'create database tess' 

psql -h postgres -p 5432 -d tess -U postgres -f init_tables.sql
