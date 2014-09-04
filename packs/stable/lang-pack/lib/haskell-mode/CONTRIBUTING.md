If you intend on submitting a pull request, whichever branch you
choose to submit a pull request on, please ensure that it is properly
rebased against master. This will ensure that the merge is clean and
that you have checked that your contribution still works against the
recent master. A typical workflow might be:

Have a remote setup that we can pull proper changes from:

    $ git remote add haskell git://github.com/haskell/haskell-mode.git

Start our topic branch:

    $ git branch my-topic-branch

Hack hack hack! Once changes committed
(c.f. [github commit message format](https://github.com/blog/926-shiny-new-commit-styles)),
run git pull on master and try to rebase onto it to check whether your
work is out of date.

    $ git commit -a -m "My topic done."
    $ git pull haskell master
    $ git checkout my-topic-branch
    $ git rebase master

If there are any conflicts, resolve them. Push changes to your Github
fork:

    $ git push origin my-topic-branch

Make a pull request on Github for my-topic-branch. Pull request gets
merged in. Pull from the official Github remote:

    $ git pull haskell master

Delete your topic branch:

    $ git branch -D my-topic-branch

I'm all done!
