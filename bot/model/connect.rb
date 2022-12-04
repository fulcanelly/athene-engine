
throw 'required PG_PASS env var' unless ENV['PG_PASS'] or ENV['PG_PASS'].empty?

ActiveRecord::Base.establish_connection(
  adapter: 'postgresql',
  database: 'tess',
  host: 'postgres',
  port: '5432',
  username: 'postgres',
  password:  ENV['PG_PASS']
)

ActiveRecord::Base.logger = Logger.new(STDOUT)