Common Lisp Programming Challenge
=================================

This repository is part of the Common Lisp Programming Challenge
announced at [spxy.org/cc/clpc/](https://spxy.org/cc/clpc/).

This repository provides some common exercises and code examples that
participants of this challenge can work through for every week of the
challenge.

Each week's exercise is composed of 5 units. Each unit is intended
to take roughly 1 hour of work. If a unit takes significantly more
than an hour (say, 2 hours or more), please [create an issue][ISSUES]
and let us know. Therefore each week may involve about 5 hours of work.
Hopefully, this allow people with busy schedules to also participate in this
challenge.

The content of this repository is available under the [CC BY 4.0][CCBY]
license.


Contents
--------

* [Week 1](#week-1)
  * [Unit 1.1: SBCL](#unit-1.1-sbcl)
  * [Unit 1.2: Emacs](#unit-1.2-emacs)
  * [Unit 1.3: SLIME](#unit-1.3-slime) (Coming up!)


Week 1
------

## Unit 1.1: SBCL

> "Lisp is worth learning for the profound enlightenment experience you
> will have when you finally get it. That experience will make you a
> better programmer for the rest of your days, even if you never
> actually use Lisp itself a lot." -- Eric S. Raymond

This unit shows how to quickly setup SBCL on your system. There are
many Common Lisp implementations to choose from. Take a look at
https://common-lisp.net/implementations for a list of some of the
popular ones. We choose SBCL for this unit because it is the most
popular free and open source implementation of Common Lisp. It is also
known for its good performance.

 1. Install SBCL.

    On macOS, it can be easily installed with Homebrew with the
    following command:

    ```sh
    brew install sbcl
    ```

    On Debian, Ubuntu, or any Debian/Ubuntu-based GNU/Linux system, it
    can be installed with the following command:

    ```sh
    apt-get install sbcl
    ```

    See http://www.sbcl.org/platform-table.html for download and
    installation details for other operating systems.

 2. Open your favorite editor, type this code, and save it as hello.lisp:

    ```lisp
    (format t "hello, world~%")
    ```

    Now enter this command to run the program:

    ```sh
    sbcl --script hello.lisp
    ```

 3. Congratulations! You have a working Common Lisp program now. Take a
    moment and [share your success on Twitter](https://twitter.com/compose/tweet?text=I+just+began+my+%23CommonLispProgrammingChallenge+with+a+%22hello%2C+world%22+program%3A%0A%0A(format+t+%22hello%2C+world~%25%22)%0A%0AJoin+this+challenge+with+%40spxycc+and+me.+Let+us+spread+the+joy+of+learning+this+beautiful+programming+language.+See+https%3A%2F%2Fspxy.org%2Fcc%2Fclcc%2F+and+learn+how.)!

 4. Open [Practical Common Lisp](http://www.gigamonkeys.com/book/)
    written by Peter Seibel and start working through the book. Get some
    head start with this book if you can afford the time right now. We
    will return to this book after going through a couple of more
    units to set up the development environment.

 5. Also, join us at our community forums, share your progress, as well
    as help and encourage others who are also participating in this
    programming challenge. Here are our community forum details:

      - Reddit: [r/spxy](https://www.reddit.com/r/spxy/)
      - Matrix: [#spxy:matrix.org](https://matrix.to/#/#spxy:matrix.org)
      - Freenode: [#spxy](https://webchat.freenode.net/#spxy)
      - Twitter: [@spxycc](https://twitter.com/spxycc)
      - Mailing list: [groups.google.com/g/spxy](https://groups.google.com/g/spxy)

    You don't have to join each one of them. Just one or two where you
    feel comfortable hanging out and share your updates, code examples,
    post links, discussion posts, questions, and answers there.

In the next few units, we will see how to set up an interactive
programming environment with Emacs and SLIME.


## Unit 1.2: Emacs

> "Calling EMACS an editor is like calling the Earth a hunk of dirt." --
> Chris DiBona

While it is perfectly possible to write Common Lisp programs and develop
complex Common Lisp projects with any editor or IDE that has good
support for Common Lisp, Emacs and SLIME is perhaps the most popular
development environment in the Lisp community. In this unit, we will
see how to set up GNU Emacs.

This unit is useful for those who have little to no Emacs experience.
If you are an experienced Emacs user, please proofread this unit and
if you find any issues, [create issues][ISSUES] or send us pull
requests.

 1. See https://www.gnu.org/software/emacs/download.html for Emacs
    download information. Here are some common commands:

    On macOS, enter the following command if you use Homebrew:

    ```
    brew cask install emacs
    ```

    On Debian, Ubuntu, etc., enter the following command if you use the
    system in terminal mode only (i.e., no GUI desktop):

    ```
    sudo apt-get install emacs
    ```

 2. Enter the following command to run Emacs:

    ```
    emacs
    ```

    On macOS, you may receive the following error message in a dialog
    box:

    ```
    “Emacs.app” can’t be opened because Apple cannot check it for
    malicious software.
    ```

    To resolve this issue, go to Apple menu > System Preferences >
    Security & Privacy > General and click `Open Anyway`.

 3. Within Emacs, enter the following command to create a file, say,
    `~/hello.txt`:

    ```
    C-x C-f ~/hello.txt
    ```

    This creates a buffer for the new file. Type some text into the
    buffer. Type out at least 3-4 words. We will need it for the next
    two steps.

    Note in the Emacs world (and elsewhere too), we use the notation
    `C-` to denote we should press the <kbd>ctrl</kbd> and hold it while
    pressing the next key. For example, what is often denoted as
    <kbd>ctrl</kbd> + <kbd>x</kbd> is denoted as `C-x` here.

 4. Move backward by one word with the following command:

    ```
    M-b
    ```

    The notation `M-` denotes pressing and holding the meta key. What's
    a meta key? It is usually the <kbd>option</kbd>/<kbd>alt</kbd>. The
    <kbd>esc</kbd> key also works as the meta key but most people use
    the <kbd>alt</kbd> or <kbd>option</kbd> key as the meta key. In
    case, you are having trouble with getting the
    <kbd>alt</kbd>/<kbd>option</kbd> key working with Emacs, read [this
    article](https://www.emacswiki.org/emacs/MetaKeyProblems).

    Similarly, move forward by one word with the following:

    ```
    M-f
    ```

 4. Then type some text in it and save the file:

    ```
    C-x C-s
    ```

 5. Quit Emacs:

    ```
    C-x C-c
    ```

The key-bindings to perform various operations like creating file,
saving file, quitting the editor, etc. may look arcane at first, but
repeated usage of the key-bindings develops muscle memory soon and after
having used them for a few days, one does not even have to think about
them. The fingers do what the mind wants effortlessly due to muscle
memory.

While you are getting used to the Emacs key-bindings,
keep this [GNU Emacs Reference
Card](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf) handy
with you. Also, if you are using it in GUI mode, then the menu options
can be quite helpful. The menu options contain frequently used
operations. The option for each operation also displays the key-bindings
that can be used to invoke the same operation.


## Unit 1.3: SLIME

Coming up!


License
-------

The content of this repository is licensed under the
[Creative Commons Attribution 4.0 International License][CCBY].

You are free to share the material in any medium or format and/or adapt
the material for any purpose, even commercially, under the terms of the
Creative Commons Attribution 4.0 International (CC BY 4.0) License.

This document is provided **as-is and as-available,**
**without representations or warranties of any kind,** whether
express, implied, statutory, or other. See the
[CC BY 4.0 Legal Code][CCBYLC] for details.

[CCBY]: http://creativecommons.org/licenses/by/4.0/
[CCBYLC]: https://creativecommons.org/licenses/by/4.0/legalcode
[ISSUES]: https://github.com/spxy/clpc/issues
