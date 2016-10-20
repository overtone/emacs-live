
MYDIR  = File.dirname(__FILE__)
TESTDIR = "#{MYDIR}/test"

namespace "test" do
  
  desc "Run tests using `emacs-snapshot'"
  task :snapshot do
    system "emacs-snapshot -Q -l #{TESTDIR}/init.el"
  end

  desc "Run tests using `emacs-22'"
  task :twenty_two do
    system "emacs22 -Q -l #{TESTDIR}/init.el"
  end
  
  desc "Run tests using `emacs'"
  task :emacs do
    system "emacs -Q -l #{TESTDIR}/init.el"
  end
  
end

task :default => 'test:emacs'
