# *btx*

This is a light-weight, declarative, command-line interface for working with BibTeX bibliographies (i.e., `.bib` files). **btx is currently a work in-progress so there are still a few rough spots**; however, much of the basic functionality is already in place. A more complete description about how *btx* works with several examples can be found in the [*btx* wiki](https://github.com/MWRuszczycky/btx/wiki).

## Introduction

The *btx* program lets you write declarative scripts to manipulate both BibTeX bibliographies and the entries they contain. For example, suppose you want to create a new `.bib` file called `animals.bib`, download a BibTeX reference for an article with a specific digital-object-identifier, rename it `Cats2016` and then edit the fields using your favorite editor, such as *Vim*. This could then all be accomplished using the following *btx* script entered at the command-line:
```
btx in animals.bib and doi 10.1016/bs.mie.2017.07.022 and name Cats2016 and edit vim
```
This breaks down as follows:
1. The `btx` command invokes *btx* at the command line as usual.
2. The `in` command sets (or creates) the working bibliography as the file `animals.bib`.
3. The `and` key-words separate the individual commands.
4. The `doi` command downloads the BibTeX reference for the publication with digital-object-identifier `10.1016/bs.mie.2017.07.022`.
5. The `name` command changes the key of the newly downloaded BibTex entry to `Cats2016`.
6. The `edit` command runs the `vim` process on the downloaded and renamed entry so you make any necessary changes using *Vim* or whatever editor you prefer.
7. Finally, the `animals.bib` file is updated with the new entry.

The `and` key-word can also be written more concisely using a comma. Therefore, the script in the above example could just as well be written as:
```
btx in animals.bib, doi 10.1016/bs.mie.2017.07.022, name Cats2016, edit vim
```
The use of `and` and `,` are completely interchangeable and can be used together. You can use this to get more natural-language like scripts such as,
```
btx in animals.bib, doi 10.1016/bs.mie.2017.07.022, name Cats2016 and edit vim
```

## BibTeX reference format

Currently *btx* requires that BibTeX references use the "braced" format, namely,
```tex
@article{Cats2016,
    author = {Norman and Felix and Hilbert},
    title = {Facile synthesis and preparation of gram quantities of nepetalactone},
    journal = {J. Import. Res.},
    volume = {22},
    pages = {4625-4637},
    year = {2016}
}
% folder = Chemistry/terpenoids
% file = Cats_2016_JImpRes.pdf
```
Metadata can be associated with the reference as comments that immediately follow the closing brace with no new lines in between (see example above). White space as well as any comment blocks that do not represent metadata are ignored.

## Installation

*btx* uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). To clone and build the repository, run the following in your terminal:
```sh
clone https://github.com/MWRuszczycky/btx.git
cd btx
stack build
```
This will build the executable isolated within the repository. To run it, create an alias for the *Stack* `exec` command:
```sh
alias btx='stack exec btx --'
```
You can then run the executable from within the repository as shown in the examples. Alternatively, you can perform a local installation using,
```sh
stack install
```
*Stack* will tell you where the binary is placed (e.g., `.local/bin` in your home directory) in case you want to later delete it, which is all you need to do to uninstall it. Now you can envoke *btx* anywhere with just `btx` and no need to create an alias.

## To do and known issues

1. "At" symbols (i.e., `@`) in comments that are separated from the references by line breaks will cause the parser to fail. This isn't a big deal, because you can still have ,`@` symbols in the BibTeX fields and the comments that immediately follow the references and are not separated by line breaks; however, it still needs to be fixed.
2. Add complete list of BibTeX entry types. Right now there are only about a half dozen.
3. Implement better handling of multiple arguments per command in scripts.
4. Add an option to run a script without writing to the files so that you can test it out before using it.
5. Implement a REPL for interactive manipulation of bibliographies.
6. The general help string needs to be written.
7. Errors should be `Text` and not `String`.
8. Several functions still need commenting.
9. More testing to try and find more problems.
10. Commands for the masked loading of all entries to the context.
