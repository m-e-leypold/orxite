Orxite --- A static site generator for Org-Mode format source
========================================================

About
-----

Orxite is a static site generator build on

- Emacs and Org-Mode
- Gnu Make
- Shell scripts and some convenient python wrappers

Orxites unique selling points are:

- Instead of text templates, Orxite uses Lisp back-quoting and
  un-quoting as template mechanism. So a template might look like
  this:

  ```emacs-lisp
  (defun my-template (content)
	(orxite-outer-template
	 `(div/content.content
	   (:h1
		(@ :id "title" :class "title")
		,(@site :title)
		,content))))
  ```
  
  If you don't like Lisp, you will hate it and then Orxite is not for you.

- Orxite doesn't have a seperate template DSL. Instead the full power
  of (Emacs) Lisp is available.
  
- Extensibility by adding make rules or Emacs Lisp procedures.

- Provides the full power of Org-mode to blogging.


License
-------

Orxite is licensed under the terms of the GPL:

    Orxite - A static site generator for org format source
    Copyright (C) 2023  M E Leypold

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

For the complete GPL license text see file [LICENSE](./LICENSE).

For ease of use Mathjax is included in the Orxite source tree. Mathjax is licensed under the Apache License

    Apache License
    Version 2.0, January 2004
	http://www.apache.org/licenses/

see file [style/MathJax-2.7.9/LICENSE](./style/MathJax-2.7.9/LICENSE).

Status
------

Orxite is definitely not ready for prime time yet:

- Some mechanisms (like image processing) are simply not here
  yet. I'll add those piece for piece when I need them.

- Currently building the complete site takes a whopping 4 seconds *per
  page* on a pretty decent machine due to emacs being invoked in batch
  mode twice for every page. Invoking the build with ```make -j16```
  reduces the total time to 25%.

  I already have some ideas how the build time can be reduced
  substantially, but, again, this will have to wait until it becomes a
  more pressing concern to me.
  
- Source code comments and formatting are atrocious. This is due to
  the fact that a lot of code has been cut & pasted from an earlier
  quick and dirty spike.

Other static site generators for org-mode
-----------------------------------------

   - Plain old
     [org-publish](https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html). Plan
     ```org-publish``` doesn't provide a way out of the box to iterate
     over all pages in a site, for example to generate a list of
     blog-articles, though, looking back, I now see ways use to Orxite
     with ```org-publish```.
	 
   - [Weblorg](https://emacs.love/weblorg/doc/index.html) uses text
     templates. This was what I wanted to avoid, though Weblorg looks
     very mature.
   
   - [org-static-blog](https://github.com/bastibe/org-static-blog). I
     don't know much about this. This is probably very Emacs centric,
     and admittedly in the long run I want Orxite to be implemented on
     top of shell tools and Common Lisp rather then Emacs lisp.
      
   - [Firn](https://github.com/theiceshelf/firn/). Certainly worth a
     look.
	 
There are more...

Long term I want to experiment with "custom" markup, embedded in
org-mode markup. That is, I want to use elements of org-mode markup
and imbue them with special meaning in the rendering step. For example
in

```
Pellentesque {{{cpp_}}} =foo::bar(baz, vroom)= ligula mollis 
ultricies. See also {{{fun_}}} =foo::bar=.
```

The ```{{{cpp_}}}``` would effect that the verbatim phrase behind it
is rendered and highlighted as C++ and the ```{{{fun_}}}``` would
create a link to the function ```foo:bar```, similar to roles in
[Sphinx](https://www.sphinx-doc.org).

 For these experiments I need full control over the rendering process.
