require 'rake'

desc "Count project files"
task :count_lines do
  sh  'find . -type f -name "*.erl" -o -name "*.hrl" ' + 
      '-o -name "*.mxml" -o -name "*.as" -o -name "Rakefile" ' + 
      '-o -name "Capfile" | xargs wc -l'
end