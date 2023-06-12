# Pipelined Spreadsheet Generation Using Haskell

## Contents

In this repository are:

- Danila's [thesis](./thesis/) contents
- [Annotation](./annotation/) (short description of my work).
- [Assessment](./assessment/) by Nickolay

## Installation

Additional packages

```sh
sudo apt-get install texlive-bibtex-extra biber
sudo apt install texlive-latex-extra
sudo apt-get install texlive-lang-cyrillic
```

## Updating clerk

I used the [git-subtree](https://manpages.debian.org/testing/git-man/git-subtree.1.en.html) command to add `clerk` here.

Here's a [tutorial](https://www.atlassian.com/git/tutorials/git-subtree) about `git-subtree`.

To update `clerk`, I run the following command.

```console
git subtree pull --prefix clerk https://github.com/deemp/clerk master --squash
```
