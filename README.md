# btx

This is a light-weight, declarative, command-line interface for working with BibTeX bibliographies (i.e., `.bib` files). **btx is currently a work in-progress so there are still a few rough spots**; however, much of the basic functionality is already in place.

* [Introduction](#introduction)
    * [Getting help](#getting-help)
    * [BibTeX reference format](#bibtex-reference-format)
    * [Installation](#installation)
* [The *btx* model-and-tutorial](#the-btx-model-and-tutorial)
    * [The bibliographies](#the-bibliographies)
        * [The *working bibliography* and the `in` command](
          #the-working-bibliography-and-the-in-command)
        * [The *import bibliography* and the `from` command](
          #the-import-bibliography-and-the-from-command)
            * [Resetting and unsetting the *import bibliography*](
              #resetting-and-unsetting-the-import-bibliography)
        * [The *export bibliography* and the `to` command](
          #the-export-bibliography-and-the-to-command)
            * [Unsetting and changing the *export bibliography*](
              #unsetting-and-changing-the-export-bibliography)
    * [The *context*](#the-context)
        * [*Context constructors*](#context-constructors)
        * [*Context operators*](#context-operators)
            * [Updating the *working bibliography*](
              #updating-the-working-bibliography)
            * [Viewing entries: `view`](
              #viewing-entries-view)
            * [Deleting entries: `pull` and `toss`](
              #deleting-entries-pull-and-toss)
            * [Renaming entries: `pull` and `name`](
              #renaming-entries-pull-and-name)
            * [Exporting and moving entries: `send`](
              #exporting-and-moving-entries-send)
            * [Importing entries: `take`](
              #importing-entries-take)
            * [Editing entries: `edit`](
              #editing-entries-edit)
        * [Queries](#queries)
            * [Summarizing the bibliographies and *context*: `info`](
              #summarizing-the-bibliographies-and-context-info)
            * [Listing entries in the *working bibliography*: `list`](
              #listing-entries-in-the-working-bibliography-list)
* [Scripting with *btx*](#scripting-with-btx)
* [To Do](#to-do)

## Introduction

The *btx* program lets you write declarative scripts to manipulate both BibTeX bibliographies and the entries they contain. For example, suppose you want to create a new `.bib` file called `animals.bib`, add two new `article` entries, rename them `Cat2016` and `Dog2018` and then edit the fields in each using your favorite editor, such as *Vim*. This could then all be accomplished using the following *btx* script entered at the command-line:
```
btx in animals.bib and new article article and name Cat2016 Dog2018 and edit vim
```
This breaks down as follows:
1. The `btx` command invokes *btx* at the command line as usual.
2. The `in` command sets (or creates) the working bibliography as the file `animals.bib`.
3. The `and` key-words separate the individual commands.
4. The `new` command creates two new `article` type BibTeX entries that are blank and have generic keys.
5. The `name` command changes the keys of the newly created references to `Cat2016` and `Dog2018`.
6. The `edit` command runs the `vim` process sequentially on each of the two references.
7. Finally, the `animals.bib` file is updated with the two new entries.

The `and` key-word can also be written more concisely using a comma. Therefore, the script in the above example could just as well be written as:
```
btx in animals.bib, new article article, name Cat2016 Dog2018, edit vim
```
The use of `and` and `,` are completely interchangeable and can be used together. You can use this to get more natural-language like scripts such as,
```
btx in animals.bib, new article article, name Cat2016 Dog2018 and edit vim
```

### Getting help

This program is written with an emphasis on help documentation. You can get a complete list of all commands using:
```
btx help
```
You can get detailed information about any one command using:
```
btx help <command>
```
for example, to get the details about the `get` command use `btx help get`.

### BibTeX reference format

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

### Installation

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
*Stack* will tell you where the binary is placed (e.g., `.local/bin` in your home directory) in case you want to later delete it, which is all you need to do to uninstall it. Now you can envoke *btx* any where with just `btx` and no need to create an alias.

## The *btx* model and tutorial

The *btx* model involves two components: the BibTeX *bibliographies* that you are working with and the *context* where you load and manipulate BibTeX entries.

### The bibliographies

There are three types of bibliography that *btx* works with:
1. The *working bibliography*, which is the focus and must always be set.
2. The *import bibliography*, which is optionally set.
3. The *export bibliography*, which is optionally set.

#### The *working bibliography* and the `in` command

The *working bibliography* must always be set and is the default bibliography to which most of the *btx* commands apply. This bibliography is set using the `in` command. For example,
```
btx in animals.bib and ...
```
has the effect of parsing the BibTeX file `animals.bib` to generate a *btx* working bibliography for manipulation. If the file `animals.bib` does not exist, then it will be created. This is how you can use `in` to create new BibTeX bibliography files.

You can also use the `in` command to *change* the working bibliography. For example, the script:
```
btx in animals.bib, new article, name Fish1989 and in plants.bib, new book, name Orchids2006
```
has the following effects (see below for specifically how the individual commands are composed together):
1. Set or create `animals.bib` as the working bibliography.
2. Create a new `article` type entry and name it `Fishes1989`.
3. Update and save the `animals.bib` BibTeX bibliography with the new `Fishes1989` entry.
4. Set or create the `plants.bib` BibTeX bibliography as the new working bibliography.
5. Create a new `book` type entry and name it `Orchids2006`.
6. Update and save the `plants.bib` bibliography with the new `Orchids2006` entry.

As a convenience, you can also run *btx* without an initial `in` command. In ths case, the current working directory will be searched for a single `.bib` file, and this will be used as the working bibliography. If there is no `.bib` file in the current working directory or there is more than one `.bib` file, then *btx* will quit with an error message.

#### The *import bibliography* and the `from` command

In addition to the *working bibliography*, you can also designate an *import bibliography* using the `from` command. The *import bibliography* serves as a source of entries that you can use to build a new, smaller bibliography. For example, suppose you have a large centralized BibTeX repository called `everything.bib` with hundreds of entries, but you are writing a paper specifically about cats that will be submitted to a journal. You don't want to send the entire `everything.bib` file in your submission and instead want to create a smaller `cats.bib` file containing only the entries `Tabbies2016` and `Calicos2014` extracted from `everything.bib`. You would do this with the following script:
```
btx in cats.bib, from everything.bib, take Tabbies2016 Calicos2014
```
Here `from` sets the import bibliography, and the `take` command designates the references to import to `cats.bib`, which is set using `in`. The `take` command is discussed further below in the section on working with *contexts*. Unlike the *working bibliography*, an *import bibliography* need not always be set. Furthermore, the *import bibliography* is never updated or changed in any way unless you also set it as the *export bibliography* (see below).

##### Resetting and unsetting the *import bibliography*

Each time the `from` command is envoked, it has no effect other than to simply reset the *import bibliography* leaving the *context* unchanged (see below). However, if you try to use `from` on the same file used for the *working bibliography*, then the effect is to simply unset the *import bibliography*. In other words, the *working* and *import* bibliographies can never reference the same file (however, the *import* and *export bibliographies* can reference the same file, see below). Likewise, if you use the `from` command without a file path argument, then it will again just unset the *import bibliography* and do nothing else.

#### The *export bibliography* and the `to` command

Finally, you can also designate an *export bibliography* using the `to` command. The `to` command works similar to `in` such that if the designated *export bibliography* file does not exist, then it will be created. The *export bibliography* serves as a target to which you can *send* references from the current *working bibliography*. Therefore, *btx* will write to the *export bibliography* (in contrast to its behavior with the *import bibliography*). This allows you to treat the *working bibilography* as your central repository from which you can create or update other BibTeX files.

For example, we could also accomplish the same goal discussed in the above example for the *import bibliography* with the script:
```
btx in everything.bib, get Tabbies2016 Calicos2014 and to cats.bib, send
```
The `get` command is discussed below but essentially designates which entries from the *working bibliography* will be exported to `cats.bib` using the `send` command. The `send` and `to` commands can also be combined into a single "sugared command" `send to` such that the following script will have the same effect:
```
btx in everything.bib, get Tabbies2016 Calicos2014 and send to cats.bib
```
The `get` and `send` commands are discussed further below.

##### Unsetting and changing the *export bibliography*

Each time the `to` command is envoked, the current *export bibliography* gets saved and the new *export bibliography* gets loaded. If the `to` file path is the same as that to the current *working bibliography* then the current *export bibliography* gets saved and then unset. In other words, the *export* and *working bibliographies* can never reference the same file. You will get the same effect by using `to` with no arguments. However, you *can* set the *export bibliography* to reference the same file as the *import bibilography*. This will allow you change the *import bibliography file* without changing the entries that *btx* *sees* in the *import bibliography* as it was previously loaded.

### The *context*

The *btx context* is where you work on bibliography entries. It consists of a list entries that is populated by extracting entry information from the *working* or *import bibliography* and depopulated by updating the *working* or *export bibliographies*. You can also query the contents of the *working bibliography*. This divides the commands:
1. *Context constructors* populate the *context* with bibliography entries.
2. *Context operators* change, depopulate or otherwise manipulate those entries already in the *context*.
3. *Queries* allow you to query information about the *context* and bibliographies leaving the *context* otherwise unchanged.

The operation of *btx* is thus modeled as function composition with the *context* serving as a list of entries that is passed from command to command as it is modified along the pipeline. This is programmatically represented as a State monad, where the bibilographies are managed as state and the context is manipulated using monadic actions and passed between them using bind.

#### *Context constructors*

*Context constructors* populate the context with entries drawn from either the *working bibliography* or the *import bibliography*. There are five such commands:
1. `get` populates the *context* with entries from the *working bibliography* without changing it.
2. `pull` is the same as `get`, but deletes the entries from the *working bibliography*.
3. `take` is the same as `get` but draws the entries from the *import bibliography*.
4. `new` populates the context with blank entries of the specified types having generic names.
5. `doi` populates the context with entries downloaded from the internet using their doi-identifiers.

All five of these commands first update the *working bibliography* with the current context before repopulating it. If no arguments are supplied to these commands, then the effect is to simply update the *working bibliography* and clear the context. If an entry is requested and not found in the given bibliography, then a "missing entry" is added to the context that is tracked but otherwise ignored (i.e., "missing entries" have no effect on bibliographies when they are updated). See below for examples using these commands.

#### *Context operators*

*Context operators* manipulate the entries in the *contex* after populating it.

##### Updating the *working bibliography*

The *working bibliography* is automatically updated at the end of each script with the final *context*. However, the `get`, `pull`, `take`, `new` and `doi` commands also have this effect on the *current context* before repopulating it with the newly specified entries.

##### Viewing entries: `view`

You can pretty-print the details of all references in the current *context* using the `view` command. Any "missing entries" will also be displayed as such. This command otherwise leaves the context unchanged. For example, if `animals.bib` contains the references `Cats2016` and `Dogs2018`, you can view their details using:
```
btx in animals.bib, get Cats2016 Dogs2018 and view
```
When the script finishes, the *working bibliography* is updated with the current context containing `Cats2016` and `Dogs2018`, which have not changed so there is no net effect. The same would happen with
```
btx in animals.bib, pull Cats2016 Dogs2018 and view:
```
Only now `Cats2016` and `Dogs2018` are first deleted from the *working bibliography* and then just written back to it. So again, the net effect is no change.

##### Deleting entries: `pull` and `toss`

The current *context* is cleared using the `toss` command. Composing this command with `pull` allows you to delete references from the working bibliography. For example, if `animals.bib` has the entry `Cats2016` that you want to get rid of, you can use:
```
btx in animals.bib, pull Cats2016 and toss
```

##### Renaming entries: `pull` and `name`

The BibTeX entry key for all entries currently in the context can be change using the `name` command. For example, if we want to rename the entries `Cats2016` and `Dogs1984` in `animals.bib` to `BigCats` and `LittleDogs`, we could use the script:
```
btx in animals.bib, pull Cats2016 Dog1984 and name BigCats LittleDogs
```
Note that we need to use the `pull` command to populate the context. If we used the `get` command, then the final `animals.bib` file would contain the two entries `Cats2016` and `BigCats` with the same fields just different names, and the same would be true for `Dogs1984` and `LittleDogs`. Note that `name` requires that there be a one-to-one correspondence between the new names supplied and the entries in the *context*. Otherwise, the *context* remains unchanged.

##### Exporting and moving entries: `send`

Entries in the current context can be used to update the *export bibliography* using `send`. The *export bibliography* is set with `to` (see above). This command also has the effect of clearing the current *context*. Therefore, the following script:
```
btx in animals.bib, get Cats2016, to some_other.bib, send
```
has the effect of *copying* `Cats2016` (if it is not missing) to `some_other.bib` so that it is now in both `animals.bib` and `some_other.bib`. However, the script:
```
btx in animals.bib, pull Cats2016, to some_other.bib, send
```
has the effect of *moving* the `Cats2016` entry from `animals.bib` to `some_other.bib`, so that `Cats2016` is now only in `some_other.bib` and no longer in `animals.bib`.

There is also syntatic sugar which allows the `send` and `to` commands to be combined so that the above script could also be written as:
```
btx in animals.bib, pull Cats 2016 and send to some_other.bib
```
As with all *btx* commands, we can use composition to export the entries and change their names. For example,
```
btx in animals.bib, get Cats2016, name BigCats and send to some_other.bib
```
After running this script, `animals.bib` and `some_other.bib` will have the same entry named with key `Cats2016` in `animals.bib` and key `BigCats` in `some_other.bib`.

##### Importing entries: `take`

You can populate the context with entries from the *import bibliography* using the `take` command after setting the *import bibliography* using `from`. For example, if you want to import `Chickens1964` and `Chipmunks1996` from `everything.bib` to `animals.bib`, you could use the script
```
btx in animals.bib, from everything.bib, take Chickens1964 Chipmunks1996
```

##### Editing entries: `edit`

**Note: the behaviour of `edit` is still unstable in that if you make an error when editing the reference so that it fails to parse, the script will quit with an error. This will be fixed soon.**

*btx* is intended to do one and only one thing, which is the management of BibTeX bibliographies and not edit the individual entries. Therefore, this task is handed off to your favorite text editor using the `edit` command. For example, suppose you have a bibliography `animals.bib` containing the reference `Cats2016` and you want to add another author, delete the `publisher` field and fix a typo in the title. If your editor of choice is *Vim*, then you could run the script
```
btx in animals.bib, get Cats2016 and edit vim
```
This will populate the context with `Cats2016` and then run *Vim* sequentially on each reference in the *context*, which is just `Cats2016`. You can then change the `author` and `title` fields and delete the `publisher` field of the entry using *Vim*. When you write and quit *Vim*, the edited entry will be reparsed and loaded into the *contex* replacing the previous version. Therefore, when the script finishes, the *working bibliography* will be updated with the edited version of `Cats2016`.

The `get` command works here, because we did not change the key so that the previous entry in the *working bibliography* with the same key is silently overwritten. If we had changed the key, then the *working bibliography* would have been updated with the new edited reference and the previous version would have been kept. You can rekey the entry using `edit` with `pull` instead of `get` (or you can just use the `pull`/`name` pattern if don't need to make any other edits). Finally, `edit` will silently skip any "missing entries" you have tried to load into the *context*.

#### Queries

The query-type commands provide information on the current bibliographies and the current *context*. These commands have no effect on the current *context*.

##### Summarizing the bibliographies and *context*: `info`

A summary of all the bibliographies including the total number of entries and file path of each is provided with the `info` command. This command also lists all the entries in the current *context*.

##### Listing entries in the *working bibliography*: `list`

You can display one-line (i.e., 80-character-max) summaries of all the entries in the *working bibliography* using the `list` command with no argument. You can also restrict the entries displayed by providing prefixes to match entry keys on. For example,
```
btx in animals.bib, list Cat Dog
```
will display a summary line for any entry in `animals.bib` whose key begins with `Cat` or `Dog`.

## Scripting with *btx*

You can also run longer *btx* scripts from a file using `run`. In this case, new line characters become synonymous with the `and` key-word and the `,` token, so that each command can just be placed on a separate line. Any extra white space is ignored. For example, suppose we have three bibliographies `animals.bib`, `everything.bib` and `some_other.bib`. We can then write the following script and save it in a file named `my_btx_script`:
```
in animals.bib
    from everything.bib
    take Cats2016 Dogs1984
    view
    edit vim
    get Cats2016
    name BigCats
    view
    send to some_other.bib
    get Fishes1999
    send
    info
```
Next we can run the script from file using the `run` command in *btx*,
```
btx run my_btx_script
```
This will have the following effects:
1. Set `animals.bib` as the *working bibliography*.
2. Set `everything.bib` as the *import bibliography*.
3. Bring the entries `Cats2016` and `Dogs1984` from `everything.bib` into the *context*.
4. Use `view` to provide verbose output about the *context*.
5. Edit both entries to, say, fix some typos in the titles.
6. Save both entries to `animals.bib` and bring `Cats2016` alone back into the *context*.
7. Rekey `Cats2016` to `BigCats`.
8. Provide some verbose output.
9. Export the rekeyed `BigCats` to `some_other.bib` as the *export bibliography*, clearing the context in the process.
10. Repopulate the context with `Fishes1999`.
11. Send `Fishes1999` to `some_other.bib` too.
12. Summarize the final context, which is empty, and the three final bibliographies.

## To do

1. Improve error handling for the `edit` command.
2. Implement a REPL for interactive manipulation of bibliographies.
3. Fix up comments.
