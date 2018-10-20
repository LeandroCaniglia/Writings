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
Software development (63.4%)
Experimental (12.2%)
Storage (8.3%)
Academic (7.1%)
Blogs (5.8%)
No longer accessible (2.5%)
Empty (0.7)
