## GitHub bindings with servant

This library defines [servant](http://hackage.haskell.org/package/servant)
types that map to (parts of) the GitHub API v3 (GH-API). It should be useful
for anyone wanting to use the GH-API in their
application. It does not aim to be complete, but should be easily extendible.

There are two purposes for writing this package: firstly, to get some experience
with writing
[servant-client](http://hackage.haskell.org/package/servant-client) code, and
secondly, to create a more extensible github client library. 

### Related projects

There are existing projects that provide a haskell client to the GH-API:
  * [github](http://hackage.haskell.org/package/github) is complete and
    based on http-client,
  * [octohat](http://hackage.haskell.org/package/octohat) only implements parts
    of the API, and is based on wreq,
  * [hgithub](http://hackage.haskell.org/package/hgithub) is very simple,
    providing access only to the repository endponts.

Please review these other projects before using servant-github, at least for now.



