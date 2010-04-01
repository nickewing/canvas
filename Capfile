# Ruby Capistrano file for deploying the app to a server.

load 'deploy'

set :application, 'nickewing.net'

default_run_options[:pty] = true
set :scm, 'git'
set :repository, 'nick@nickewing.net:/home/nick/repos/canvas'
set :branch, 'master'
set :deploy_via, :remote_cache
ssh_options[:forward_agent] = true
set :deploy_to, '/home/nick/nickewing.net/canvas'

set :spinner, false
set :use_sudo, false

role :app, "nickewing.net"
# role :web, "nickewing.net"
# role :db,  "nickewing.net", :primary => true

DB = 'canvas'

namespace :deploy do
  
  task :finalize_update, :except => {:no_release => true} do
    run "chmod -R g+w #{latest_release}" if fetch(:group_writable, true)
    
    run "ln -s #{shared_path}/deps #{latest_release}/server/"
    
    run "cd #{latest_release}/server/; rake"
  end
  
  task :start, :roles => :app do
    run "cd #{latest_release}/server/; rake start"
  end

  task :stop, :roles => :app do
    run "cd #{latest_release}/server/; rake stop"
  end

  desc "Restart Application"
  task :restart, :roles => :app do
    stop
    start
  end
  
end

namespace :sync do
  
  task :database do
    root = File.dirname(__FILE__)
    
    dump_tar      = "#{DB}_#{Time.now.to_f}.sql.tar"
    dump_bz2      = "#{dump_tar}.bz2"
    dump_tar_path = "/tmp/#{dump_tar}"
    dump_bz2_path = "/tmp/#{dump_bz2}"
    
    on_rollback do
      run "rm -f #{dump_tar_path} #{dump_bz2_path}"
    end
    
    # dump and download
    run "pg_dump #{DB} -U #{DB} -F t -f #{dump_tar_path}"
    run "bzip2 -9 #{dump_tar_path}"
    get dump_bz2_path, dump_bz2
    run "rm -f #{dump_tar_path} #{dump_bz2_path}"
    
    # load on local
    system "bunzip2 -f #{dump_bz2}"
    system "dropdb -U #{DB} #{DB}"
    system "createdb -U #{DB} #{DB}"
    system "pg_restore -U #{DB} -d #{DB} -F tar '#{dump_tar}'"
    
    # migrate
    system "sequel19 postgres://#{DB}:#{DB}@localhost/#{DB} " +
           "-m #{root}/server/priv/db/migrations"
    
    # cleanup local
    system "rm -f #{dump_bz2} #{dump_tar}"
  end
  
end

