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

