# GitHub Notes

Root Structure
--
**Primitive objects:** User and Project
User has a Profile
Profile has Repos

OAuth
--
*Open Authentication*. There are two versions 1 and 2, which are different. Main difference: OAuth1 does not assume a secure connection and therefore implements its own security mechanisms (hence, more complex to implement). OAuth2, on the other hand, assumes SSL and is much simpler. There is still lack of agreement on adoption (check)

Mining Tools
--
There are all sorts of facilities provided by GitHub and 3rd party projects, e.g., GHTorrent.

Quality Perils
--
https://kblincoe.github.io/publications/2014_MSR_Promises_Perils.pdf

A repository is not necessarily a project
-
You need the base and all the associated forks to measure project activity.

Most projects have very few commits
-
Mean = 6 and P90 = 50. The Lorenz curve shows that very few projects get the bulk of commits.

Most projects are inactive
-
Average project activity time = 10 days. P75 = 100 days (meaning that 25% have lasted for 100 days or more)

A large portion of repositories are not for software development
-
- Software development (63.4%)
- Experimental (12.2%)
- Storage (8.3%)
- Academic (7.1%)
- Blogs (5.8%)
- No longer accessible (2.5%)
- Empty (0.7)

Two thirds of projects (71.6% of repositories) are personal
-
Committers are users with write access to the repository. If there is only 1 committer, there is no collaboration.

Only a fraction of projects use pull requests
-
And of those that use them, their use is very skewed.
The median number of pull requests per project is 2 (44.7% of projects have only 1, and 95% have 25 or less)

If the commits in a pull-request are reworked only the resulting commits are registered
-
GitHub records only the commits that result from peer-revisions, not the original commits.

It is common in projects to require a commit squash (merging all different commits into a single one) before the set of commits is merged with the main repository. In other words, some estimation needs to be included for non-observable commits.

Most  pull  requests  appear  as  non-merged, even if they are actually merged
-
At least in some projects, one cannot rely on GitHub’s _Merged_ attribute of a pull request. Therefore, one have to use some heuristics to obtain better merge estimates.

Many interesting repositories, especially those that migrated to GitHub, have an external issue database.

Many active projects do not conduct all their software development in GitHub
--
_“Any  serious  project  would  have  to  have  some
separate infrastructure - mailing lists, forums, irc
channels and their archives, build farms, etc.  [...]
Thus while GitHub and all other project hosts are
used  for  collaboration,  they  are  not  and  can  not
be a complete solution.”_

When committers are not GitHub users or the project description states somehow that the project is a mirror, most likely, there will be development activities not observable from GitHub.
