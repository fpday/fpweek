# FP Week

FP Week is a 3-days event on Functional Programming combining half-day master classes from renowned experts in their field and half-day open space sessions to foster interaction among attendants.

## Technical Details

This is a [hakyll]() based web site inspired by [github.com/t413/SinglePaged](https://github.com/t413/SinglePaged) template which uses jekyll.

The `master` branch contains the source files and a submodule containing the `gh-pages` into `_site`. To build the web site:

* Download and install [stack](http://docs.haskellstack.org/en/stable/README/)
* Run `stack build` to create the `site` executable somewhere under `.stack-work` directory
* Link it locally to simplify command: `ln -s <path-to-exe> site`
* Run `./site build` to build pages into `_site`


