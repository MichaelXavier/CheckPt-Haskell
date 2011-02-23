CheckPt Haskell
---------------

My attempt at a Haskell implementation of my CheckPt project.

It will provide a CLI for managing media items to be consumed (books, movies,
television). The workflow will be optimized for media items as files, but
tangible items such as physical books can be used as well.

Planned Usage
=============
*NOTE*: This stuff is subject to change.

*Show entire list (implemented)*
checkpt list

*List (root level) Items (implemented)*
checkpt list -r

*Add a root item (implemented)*
checkpt add "Fallen Dragon"

*Add a collection if it exists, list it otherwise (implemented)*
checkpt collection "Mad Men"

*Add a collection and items if it doesn't exist (implemented)*
checkpt collection "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Complete a root level item (implemented)*
checkpt complete "Fallen Dragon"

*Complete item(s) under a collection (implemented)*
checkpt complete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Uncomplete a root level item (implemented)*
checkpt uncomplete "Fallen Dragon"

*Uncomplete item(s) under a collection (implemented)*
checkpt uncomplete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

*Delete a root level item (implemented)*
checkpt delete "Fallen Dragon"

*Delete item(s) under a collection (implemented)*
checkpt delete "Mad Men" "Ep 1" "Ep 2" "Ep 3"

TODO
====
* Finish the CLI
* Test the CLI
* Tab completion to make complex names useful
* Use a proper hash (Data.Map?) for unique items and media collections
* Proper documentation
* Unambiguous way to complete/uncomplete/delete full media collecitons

Disclaimer
==========
As with all of the various CheckPt implementations I've created, this is mostly
a learning experience for me, so do not expect the file to work (well) unless
otherwise stated.