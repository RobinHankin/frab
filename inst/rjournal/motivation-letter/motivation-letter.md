---
output:
  pdf_document: default
  html_document:
    df_print: paged
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor   
The R Journal  
\bigskip

Dear Professor Loo,
\bigskip

Thank you for your advice the other day on my `frab` manuscript.  I
have reworked it as you suggest and believe it is stronger and more
effective as a result.

* Rewording for clarity
* Inclusion of introduction to problem by way of an illustrative R
  session showing the motivation for the package
* Discussion of `dplyr::bind_rows()` as suggested [although I would
  suggest that most of the confusion surrounding the package may be
  "adding rows" and indeed the multiple definitions of "table"].
* Discussion of related R functionality added, including a section on
  SQL formalism.  One unexpectedly difficult challenge for me
  developing the `frab` package is search engine discoverability.
  Searching for phrases such as _add two tables_ or _combine two
  tables_ returns a whole bunch of generally unrelated `SQL` hits.
  Part of the problem is that the word "table" means different things
  to the R and SQL communities.
* After long thought, I have removed the section on disordR
  discipline.  The `disordR` package is a low-level R infrastructure
  project that obviates a particular type of math/coding error.  Its
  use has saved my bacon more than once, but on sober reflection
  emphasising this aspect of the `frab` package is a distraction.
  Disord discipline is difficult to understand and, if implemented
  correctly, transparent to the user [unless it flags a violation, in
  which case it generally highlights a user-level logic error].  As
  such, it either: (a), deserves an R Journal article of its own or
  (b), should be relegated to a footnote.  I've chosen the latter,
  with a heavy heart.
* added a conclusions section

\bigskip
\bigskip

Regards,
    
    
    
    
Robin Hankin  
University of Stirling  
`hankin.robin@gmail.com`  

\bigskip

