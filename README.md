`morela` transforms a plain text description of relational database schema
(including tables, their attributes and integrity constraints) into a visual
diagram modeling the description. The visualization is produced 
using GraphViz.

This project is based on the [`erd`](http://github.com/BurntSushi/erd) tool
by Andrew Gallant.

At present `morela` is in early development phase and highly unstable.

### Installation

`morela` requires [GraphViz](http://www.graphviz.org/download/).

#### Stack

Install the [Stack](http://docs.haskellstack.org/en/stable/README/) build tool,
and build from source:

    git clone git://github.com/kukimik/morela
    cd morela
    stack install

`stack install` will put the binary into Stack's standard binary
installation path.  Unless you've overridden it, that's `~/.local/bin`
on Unix and OS X, `%APPDATA%\local\bin` on Windows.

### Usage

```bash
morela < input.mrl > output.pdf
```
