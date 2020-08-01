# *btx*

This is a light-weight, command-line interface for working with basic *BibTeX* bibliographies (i.e., `.bib` files). A short tutorial describing how *btx* works can be found in the [*btx* wiki](https://github.com/MWRuszczycky/btx/wiki), and detailed help information can be found by running `btx help`.

**Note:** The parser used by *btx* will not work with all *BibTeX* files, especially those that do not use the braced-format (see below). Likewise, *btx* collects and reformats all data between entries as metadata associated with the preceding reference entry. So, **please play around with it using a test bibliography before trying it with anything you care about**.

## Introduction

The *btx* program lets you write scripts to manipulate both *BibTeX* bibliographies and the entries they contain. For example, suppose you want to create a new `.bib` file called `animals.bib`, download a *BibTeX* reference for an article with a specific digital-object-identifier, rename it `Cats2016` and then edit the fields using your favorite editor, such as *Vim*. This could then all be accomplished using the following *btx* script entered at the command-line:
```
btx in animals.bib and doi 10.1016/bs.mie.2017.07.022 and name Cats2016 and edit vim
```
This breaks down as follows:
1. The `btx` command invokes *btx* at the command line as usual.
2. The `in` command sets (or creates) the working bibliography as the file `animals.bib`.
3. The `and` key-words separate the individual commands.
4. The `doi` command downloads the *BibTeX* reference for the publication with digital-object-identifier `10.1016/bs.mie.2017.07.022`.
5. The `name` command changes the key of the newly downloaded *BibTeX* entry to `Cats2016`.
6. The `edit` command runs the `vim` process on the downloaded and renamed entry so you make any necessary changes using *Vim* or whatever editor you prefer (e.g., you could replace `edit vim` with `edit nano`).
7. Finally, the `animals.bib` file is updated with the new entry.

The `and` keyword can also be written more concisely using a comma. Therefore, the script in the above example could just as well be written as:
```
btx in animals.bib, doi 10.1016/bs.mie.2017.07.022, name Cats2016, edit vim
```
The use of `and` and `,` are completely interchangeable and can be used together. You can use this to get more natural-language like scripts such as,
```
btx in animals.bib, doi 10.1016/bs.mie.2017.07.022, name Cats2016 and edit vim
```

## *BibTeX* reference entry format

The parser used by *btx* requires that any `@STRING` or `@PREAMBLE` entries precede any normal reference entries. Furthermore, all reference entries must follow the braced-format such as in the following example:
```tex
@article{Cats2016,
    author = {Norman and Felix and Hilbert},
    title = {Facile synthesis and preparation of gram quantities of nepetalactone},
    journal = {J. Import. Res.},
    volume = {22},
    pages = {4625-4637},
    year = {2016}
}
folder = Chemistry/terpenoids
file = Cats_2016_JImpRes.pdf
```
Any comment lines (other than those containing only spaces) or `@COMMENT` entries that follow a reference entry are treated as metadata associated with the preceding entry on a line-by-line basis. Any comment lines, `@STRING`, `@PREAMBLE` or `@COMMENT` entries that precede the first entry are treated as metadata associated with the whole bibliography and retained. Finally, *btx* will treat the `#`-character as any other.

## Installation

*btx* uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). To clone and build the repository, run the following in your terminal:
```sh
clone https://github.com/MWRuszczycky/btx.git
cd btx
stack build
```
This will build the executable isolated within the repository. If you get an error at this point regarding buildable sublibraries but no buildable libraries, then you may need to upgrade your version of *Stack* by first running `stack update` and then `stack upgrade` (I can get it to compile with *Stack*-1.9.3).

Once you have compiled the binary, you can run it by creating an alias for the *Stack* `exec` command:
```sh
alias btx='stack exec btx --'
```
You can then run the executable from within the repository as shown in the examples. Alternatively, you can perform a local installation using,
```sh
stack install
```
*Stack* will tell you where the binary is placed (e.g., `.local/bin` in your home directory) in case you want to later delete it, which is all you need to do to uninstall it. Now you can envoke *btx* anywhere with just `btx` and no need to create an alias.

A test-suite is also currently under development. You can run the tests so far implemented with
```sh
stack test
```

## To do and known issues

* If an entry is going to be replaced, then provide a warning and some mechanism to change its key. This may require changing some of the commands. For example, `pull` becomes `get` and `get` becomes `copy` so that you are not annoyed by unnecessary replacement messages and don't have to use `pull` all the time.
* Remove the `+` token and just require `,` in scripts even in files. The `+` interferes with some program names such as `notepad++`. This includes a refactor of the script parser.
* Allow more than one reference to be edited at a time. This could be done by adding an `edits` command.
* Add a configuration file to name a default bibliography file and a set of shortcuts to bibliography files.
* Add commands for resorting, adding and deleting the fields of references.
* Refactor the BibTeX parser to allow for quoted fields.
