CheckPt Haskell
---------------

My attempt at a Haskell implementation of my CheckPt project.

It will provide a CLI for managing media items to be consumed (books, movies,
television). The workflow will be optimized for media items as files, but
tangible items such as physical books can be used as well. This is basically a
2-level checklist.

CheckPt will create a file .checkpt in your home directory which you can
symlink to DropBox or what have you.

Installation
============
For the time being, this can be installed with `make install`. Check
CheckPt.cabal for cabal prerequisites.

To get started, run `checkpt init`.

Usage
=====
*Show entire list*

checkpt list

*List (root level) Items*

checkpt list -r

*Add a root item*

checkpt add "Fallen Dragon"

*Add a collection if it exists, list it otherwise*

checkpt collection "Mad Men"

*Add a collection and items if it doesn't exist*

checkpt collection "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Complete a root level item*

checkpt complete "Fallen Dragon"

*Complete item(s) under a collection*

checkpt complete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Uncomplete a root level item*

checkpt uncomplete "Fallen Dragon"

*Uncomplete item(s) under a collection*

checkpt uncomplete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Delete a root level item*

checkpt delete "Fallen Dragon"

*Delete item(s) under a collection*

checkpt delete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

Ideas
=====
* Tab completion to make complex names useful
* Use a proper hash (Data.Map?) for unique items and media collections
* Unambiguous way to complete/uncomplete/delete full media collecitons

Disclaimer
==========
As with all of the various CheckPt implementations I've created, this is mostly
a learning experience for me, so do not expect this software to work perfectly
or make your life a fantastical world of sunshine and gumdrops.
