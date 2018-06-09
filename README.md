# monadf

monadf is a very simple Parsec based Brainf*** interpreter. It implements some very basic optimizations, but is relatively slow still.

## Using monadf

To install globally, clone the repo and run 

    stack install

Now you can run it anywhere using `mdbf`. If you want to evaluate a file, use the `eval` command.

    mdbf eval <path>
    
Example:

    mdbf eval ./bf-scripts/mandlebrot.b
    
Depending on your computer you might be able to slowly render a mandlebrot render, courtesy of Erik Bosman.

Besides evaluating files, you can also go into a repl. To do that run `mdbf repl`, and you'll go into a very basic repl instance.
