Information
===========

Summary
-------

This folder contains the Haskell for defining any page's CSS. The convention
is that the file name is the same as the page for which it defines the CSS.
See the notable files for information on the files which do not follow this
convention.

##### Notable Files

Common.hs defines the CSS that is common across all pages, such as the header
and disclaimer at the bottom of the page. This also contains the CSS for the
nodes in the graph modals.

Compiler.hs merges all the other Haskell files in the folder, and then writes
it to public/style/app.css.

Constants.hs defines the constants utilized by the other CSS files.

Search.hs defines the CSS for the search page, the important distinction being
that this is only for the search page, and not the other search bars that are
present in other pages such as the draw page.