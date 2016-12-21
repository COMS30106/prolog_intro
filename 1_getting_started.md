# Finding your way around SWI-Prolog #
## Getting started ##
Log on to a (Linux) lab machine and start up SWI-Prolog with the command **swipl**. (The location of the most recent swipl binary is `/lib64/swipl-7.2.3/bin/x86_64-linux/swipl` but it's linked to `/bin/swipl`)

SWI-Prolog starts up as follows:

    Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 7.2.3)
    Copyright (c) 1990-2015 University of Amsterdam, VU Amsterdam
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
    and you are welcome to redistribute it under certain conditions.
    Please visit http://www.swi-prolog.org for details.

    For help, use ?- help(Topic). or ?- apropos(Word).

    ?-

'**?-**' is the Prolog prompt at which you can type a query, ended by a period. Prolog programs should be stored in a file and then read into the Prolog interpreter, by means of the query **?-consult(file).** (which can be abbreviated **?-\[file\].**). The default extension of a SWI-Prolog file is **.pl**, so the query **?-consult(file).** will either load **file** or **file.pl**. If your filename contains non-alphanumeric characters you should use single quotes (e.g. **?-consult('file.txt').** or **?-\['file.txt'\].**).

## Some other useful commands from **pl** ##
The following command terms can be typed into the **pl** command line shell.

### `?-consult(user).` or `?-[user].` ###
Type straight into the database. Type &lt;Ctrl-d&gt; to stop. This is only useful for trying things out quickly; consulting a file is the normal way to edit a Prolog program.

### `?-listing.` ###
Display a listing of the current contents of the Prolog database.

### `?-make.` ###
Look for changes in file dates of the consulted Prolog program (or programs) and reconsult them if changes have occurred.

### `?-help(Topic).` ###
Display manual at the specified topic. e.g. `help(bagof).`

### `?-apropos(Word).` ###
Display manual and do a free text search for the word. e.g. `apropos(append).`

### `?-halt.` ###
Close the SWI-Prolog shell - without confirmation!

**pl** shell can also be closed with &lt;Ctrl-d&gt;.

### `?-debug.` and `?-nodebug.` ###
See [this page](2_tracer_debugger) for details.

### `?-trace.` and `?-notrace.` ###
See [this page](2_tracer_debugger) for details.

## Using `SWISH` environment ##
Another way of executing Prolog code is to create a SWISH notebook ([read more](https://github.com/SWI-Prolog/swish)), which is equivalent to Jupyter environment in Python.  
To run it, first go to `cd /opt/swish/` and then run `swipl run.pl`. This should start a SWISH server, so now you can open your favourite web browser and go to [`http://localhost:3050/`](http://localhost:3050/).

SWISH examples and help are available through its interface.

## Using swipl on your own computer ##
**Important:** Please note that your **assignments must work on the lab machines** even if you develop them on your own computer. SWI-Prolog is highly portable between platforms provided that you do not use platform-specific features (e.g. such as calling MS Windows API). None of the assignments require the use of platform-specific features.
