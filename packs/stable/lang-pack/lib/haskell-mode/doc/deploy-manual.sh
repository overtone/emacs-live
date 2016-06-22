#!/bin/bash

set -e
set -u

if [[ "${TRAVIS_REPO_SLUG:-}" != "haskell/haskell-mode" ]]; then
    echo "TRAVIS_REPO_SLUG is '${TRAVIS_REPO_SLUG:-}' expected 'haskell/haskell-mode'"
    echo "Manual deployment available only directly for 'haskell/haskell-mode' repo"
    exit 0
fi

if [[ "${TRAVIS_BRANCH:-}" != "master" ]]; then
    echo "TRAVIS_BRANCH is '${TRAVIS_BRANCH:-}' expected 'master'"
    echo "Manual deployment available only for 'master' branch"
    exit 0
fi

if [[ -z "${GITHUB_DEPLOY_KEY_PASSPHRASE:-}" ]]; then
    echo "GITHUB_DEPLOY_KEY_PASSPHRASE must be set to passphrase for github deploy key"
    echo "Pull requests do not have access to secure variables"
    exit 0
fi

# Note: GITHUB_DEPLOY_KEY_PASSPHRASE comes from 'secure' section in .travis.yml
cp haskell-mode-travis-deploy-key haskell-mode-travis-deploy-key-plain
chmod 0600 haskell-mode-travis-deploy-key-plain
ssh-keygen -f haskell-mode-travis-deploy-key-plain -P $GITHUB_DEPLOY_KEY_PASSPHRASE -p -N ""

eval $(ssh-agent)
ssh-add haskell-mode-travis-deploy-key-plain

# Git setup, this commit should appear as if Travis made it
export GIT_COMMITTER_EMAIL='travis@travis-ci.org'
export GIT_COMMITTER_NAME='Travis CI'
export GIT_AUTHOR_EMAIL='travis@travis-ci.org'
export GIT_AUTHOR_NAME='Travis CI'

HEAD_COMMIT=$(git rev-parse --short HEAD)

if [ -d gh-pages-deploy ]; then
    rm -fr gh-pages-deploy
fi

git clone --quiet --branch=gh-pages "git@github.com:haskell/haskell-mode.git" gh-pages-deploy

cd gh-pages-deploy
git rm -qr manual/latest
cp -r ../html manual/latest
find manual/latest -name '*.html' -exec sed -i -e '/^<\/head>$/i\
<script src="../../index.js"> </script>
' \{} \;
git add manual/latest
(git commit -m "Update manual from haskell/haskell-mode@${HEAD_COMMIT}" && git push origin gh-pages) || true
cd ..
rm -fr gh-pages-deploy

eval $(ssh-agent -k)
echo Done!
