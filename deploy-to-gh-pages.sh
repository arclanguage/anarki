#!/usr/bin/env bash

# We take the built `build-gh-pages` content from the `master` branch
# and deploy it as a commit to the `qh-pages` branch.

# This is adapted from
# <https://github.com/steveklabnik/automatically_update_github_pages_with_travis_example>.

# If there's an error during any command, exit the script. If any
# unset environment variable is used, exit the script.
set -o errexit -o nounset


# We only proceed only under a specific set of conditions. Most of
# these messages are worded as though someone will be reading them
# from a Travis build log.
#
# In a Travis build, most of these variables will be set to something.
#
# If SHOULD_COMMIT_TO_GH_PAGES isn't set, that's probably because the
# build matrix in .travis.yml was accidentally updated in a way that
# omitted that variable.
#
# If ROCKETNIABOT_GH_TOKEN isn't set, that's probably because an entry
# hasn't been configured for it in Travis. See the notes below about
# how this should be set up.

# NOTE: In Bash, "${foo-}" substitutes the value of foo if it's set
# and the value "" if it isn't. We're using it here as a way to work
# around the `nounset` option specifically for the purpose of giving a
# better status message.

if [ "${TRAVIS-}" == "" ]; then
  echo \
    'This script is designed to run only during the continuous' \
    'integration tests. Aborting.'
  exit 1
fi

if ! (
  [ "${TRAVIS_REPO_SLUG-}" == "arclanguage/anarki" ] &&
  [ "${TRAVIS_BRANCH-}" == "master" ]
); then
  echo \
    'Not a build of the arclanguage/anarki repo'\''s master branch.' \
    'Aborting.'
  exit 0
fi

if [ "${TRAVIS_PULL_REQUEST-}" != "false" ]; then
  echo 'The build was initiated by a pull request. Aborting.'
  exit 0
fi

if [ "${SHOULD_COMMIT_TO_GH_PAGES-}" != "true" ]; then
  echo \
    'The build was initiated by a build matrix entry that was not' \
    'designated for committing to the `gh-pages` branch. Aborting.'
  exit 0
fi

if [ "${ROCKETNIABOT_GH_TOKEN-}" == "" ]; then

  # This variable should be bound to a GitHub personal access token
  # with the public_repo permission. The token should be configured in
  # Travis CI as an "encrypted environment variable" so that
  # contributors typically don't have access to its value unless they
  # push a build script designed to reveal it.
  #
  # If someone ever gains access to this token, please revoke both
  # that user's and rocketniabot's contributor access to the repo to
  # inhibit abuse until the situation can be worked out, and inform
  # GitHub user rocketniabot (well, rocketnia) about this so they can
  # revoke the token.
  #
  # The user who grants this personal access token essentially takes
  # responsibility for the automated pushes, as far as GitHub is
  # concerned. If this role changes hands, please rename the variable.
  #
  # If at times nobody is available and willing to take responsibility
  # for automated pushes, it's no great loss; the `gh-pages` branch
  # will just need to be updated manually.

  echo \
    'The Travis CI settings for the repo do not have a' \
    'ROCKETNIABOT_GH_TOKEN environment variable configured. A' \
    'GitHub personal access token is necessary in order to push the' \
    'build to GitHub. Aborting.'
  exit 0
fi


# We clone the `gh-pages` branch. If it doesn't exist yet, we create a
# new branch with an empty history.
git clone "https://$ROCKETNIABOT_GH_TOKEN@github.com/arclanguage/anarki.git" build-gh-pages/repo
cd build-gh-pages/repo
git checkout gh-pages || git checkout --orphan gh-pages

# We replace all the files with the contents of the
# build-gh-pages/site/ directory, plus a short readme. We stage all
# the files for a commit.
git rm -rf .
cp -r ../site/* .
echo 'This `gh-pages` branch is generated and deployed automatically when commits are made to the Anarki `master` branch. See the scripts .travis.yml and deploy-to-gh-pages.sh on `master`.' > README.md
git add .

# We make a commit to the `gh-pages` branch that looks a lot like the
# latest commit to `master`.
git config user.name "$(git log -1 --pretty=%an master)"
git config user.email "$(git log -1 --pretty=%ae master)"
git commit -m "$(git log -1 --pretty=%B master)"

# We push this new commit to the `gh-pages` branch. We specify the
# remote branch as `refs/heads/gh-pages` so that we create the branch
# if it doesn't exist yet.
git push -q origin HEAD:refs/heads/gh-pages
