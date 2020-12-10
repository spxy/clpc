Common Lisp Programming Challenge
=================================

Learn Common Lisp (CL) together with a community of other learners and
experts.

If you are a beginner to CL, this challenge will help you to get started
with setting up a development environment from scratch and begin the
journey of learning CL together with other community members. If you are
an expert at CL, you are welcome to hang out with us in our
[forums](#forums) and help others with their questions.

This repository provides some setup exercises and code examples that
participants of this challenge can work through for every week of the
challenge.

Each week's exercise is composed of 5 units. Each unit is intended
to take roughly 1 hour of work. Therefore each week may involve about 5
hours of work. Hopefully, this will allow people with busy schedules to
participate in this challenge too.

The content of this repository is available under the [CC BY 4.0][CCBY]
license.

Note: Since this is the first iteration of this challenge, content for
every new week will appear every two weeks. Thus, the first iteration
will complete in roughly two months. That's because it is the holiday
season, so we are taking it slow. From the second iteration onwards,
when all content is already ready and may at most require minor
tweaking, each iteration of this challenge will run for one month.


Contents
--------

* [Week 1](#week-1)
  * [Unit 1: SBCL](#unit-1-sbcl)
    * [Unit 1.1: Basics](#unit-11-basics)
    * [Unit 1.2: Install SBCL](#unit-12-install-sbcl)
  * [Unit 2: Emacs](#unit-2-emacs)
    * [Unit 2.1: Install Emacs](#unit-21-install-emacs)
    * [Unit 2.2: Emacs Init File](#unit-22-emacs-init-file)
    * [Unit 2.3: Emacs Help](#unit-23-emacs-help)
    * [Unit 2.4: More Emacs Commands](#unit-24-more-emacs-commands)
  * [Unit 3: SLIME, Paredit, Rainbow Delimiters](#unit-3-slime-paredit-rainbow-delimiters)
    * [Unit 3.1: Install Packages](#unit-31-install-packages)
    * [Unit 3.2: Use SLIME](#unit-32-use-slime)
    * [Unit 3.3: Use Paredit](#unit-33-use-paredit)
    * [Unit 3.4: Use Rainbow Delimiters](#unit-34-use-rainbow-delimiters)
  * [Unit 4: Paredit](#unit-4-paredit) (Coming up!)
* [Forums](#forums)
* [License](#license)


Week 1
------

In the first week, we set up all the tools we need to learn and program
in Common Lisp productively. For someone who has not worked with these
tools before, these may all seem like too many tools with too many new
key-bindings to learn. Do not worry! The important key-bindings and a
quick guide to get started with each tool are provided in the sections
below. They will get you off the ground as quickly as possible.

Mastering these tools is a life-long endeavour though. Expert users of
these tools use these tools daily for months and years before they
become experts, so do not fret if they appear needlessly cumbersome in
the beginning. Just keep using them, keep practising, and keep learning
about them as you go further and further in this journey of learning
Common Lisp programming.


## Unit 1: SBCL

> "Lisp is worth learning for the profound enlightenment experience you
> will have when you finally get it. That experience will make you a
> better programmer for the rest of your days, even if you never
> actually use Lisp itself a lot." -- Eric S. Raymond

This unit shows how to quickly setup SBCL on your system.


### Unit 1.1: Basics

First, a couple of basics:

 1. Lisp is not a single programming language. It is a family of
    programming languages with a distinctive fully parenthesized prefix
    notation. Some popular dialects of Lisp are Common Lisp, Scheme,
    Clojure, Emacs Lisp, Racket, etc.

 2. Common Lisp is a dialect of Lisp with an ANSI standard. There are
    many implementations of Common Lisp in the form of compilers as well
    as interpreters. Some examples are Steel Bank Common Lisp (SBCL),
    Clozure CL (CCL), CLISP, etc. Take a look at
    https://common-lisp.net/implementations for examples of more
    implementations. Clozure CL should not be confused with Clojure.
    Clozure CL is a compiler implementation for Common Lisp whereas
    Clojure is a separate dialect Lisp meant for the Java platform.

In this document, we work primarily with Common Lisp only. We will work
a little bit with Emacs Lisp too in order to configure Emacs as a Common
Lisp development environment but that will be very brief. We will spend
most of our time with Common Lisp. We choose SBCL as the implementation
we will work with. It is the most popular free and open source
implementation of Common Lisp. It is also known for its good
performance. Note that since Common Lisp is an ANSI standard, any Common
Lisp implementation is okay for this challenge, however, the
instructions provided below are specific to SBCL only.


### Unit 1.2: Install SBCL

Perform the following steps to get started with SBCL:

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

 5. Also, join us at our [forums](#forums), share your progress, as well
    as help and encourage others who are also participating in this
    programming challenge. Section [forums](#forums) has our forum
    details. You don't have to join each one of them. Just one or two
    where you feel comfortable hanging out. You are welcome to share
    your updates, code examples, post links, discussion posts,
    questions, and answers in these forums.

In the next few units, we will see how to set up an interactive
programming environment with Emacs and SLIME.


## Unit 2: Emacs

> "Calling EMACS an editor is like calling the Earth a hunk of dirt." --
> Chris DiBona

While it is perfectly possible to write Common Lisp programs and develop
complex Common Lisp projects with any editor or IDE that has good
support for Common Lisp, Emacs and SLIME are the most popular
development environment in the Lisp community. In this unit, we will see
how to set up GNU Emacs.


### Unit 2.1: Install Emacs

This unit is useful for those who have little to no experience with
Emacs. If you are an experienced Emacs user, please proofread this unit
and if you find any issues, [create issues][ISSUES] or send us pull
requests. Perform the following steps to get started with Emacs:

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

 3. Within Emacs, enter the following command to open file, say,
    `~/hello.txt`:

    ```
    C-x C-f hello.txt RET
    ```

    A new buffer to edit `~/hello.txt` is created. If a file with that
    name exists on your file system, then it loads the content of the
    file into the buffer.

    Note that in the Emacs world (and elsewhere too), we use the
    notation `C-` to denote that we should press the <kbd>ctrl</kbd> key
    and hold it while pressing the next key. For example, what is often
    denoted as <kbd>ctrl</kbd> + <kbd>x</kbd> is denoted as `C-x` here.

    Similarly, `RET` denotes the <kbd>enter</kbd>/<kbd>return</kbd> key.

    Another important thing to note is that when a command has two
    consecutive `C-` key sequences, you don't really have to press and
    hold the <kbd>ctrl</kbd> key twice. Instead, you can press and hold
    <kbd>ctrl</kbd> key once at the beginning, then press the two other
    keys, and then release the <kbd>ctrl</kbd> key. This shortcut is
    more convenient to work with. For example, for `C-x C-f`, you can
    press and hold <kbd>ctrl</kbd>, then press <kbd>x</kbd>, then press
    <kbd>f</kbd>, and then release <kbd>ctrl</kbd>. In other words,
    think of `C-x C-f` as `C-(x f)`.

 4. Now type some text into the buffer. Type out at least 3-4 words. We
    will need it for the next two steps.

 5. Move backward by one word with the following command:

    ```
    M-b
    ```

    The notation `M-` denotes pressing and holding the meta key. What's
    a meta key? It is usually the <kbd>option</kbd>/<kbd>alt</kbd> key
    on the keyboard. The <kbd>esc</kbd> key also works as the meta key
    but most people use the <kbd>alt</kbd> or <kbd>option</kbd> key as
    the meta key. In case, you are having trouble with getting the
    <kbd>alt</kbd>/<kbd>option</kbd> key working with Emacs, read [this
    article](https://www.emacswiki.org/emacs/MetaKeyProblems).

    Similarly, move forward by one word with the following:

    ```
    M-f
    ```

 6. The `C-g` key sequence cancels the current command. This can be used
    when you mistype a command and want to start over or if you type a
    command partially, then change your mind and then you want to cancel
    the partially typed command. Try out these examples:

    ```
    C-x C-f C-g
    ```

    ```
    C-x C-g
    ```

 7. Save the buffer to a file on the file system with this command:

    ```
    C-x C-s
    ```

 8. Quit Emacs:

    ```
    C-x C-c
    ```

Now you know how to start Emacs, open a file, save it, and quit. Improve
your Emacs knowledge further by taking the Emacs tutorial that comes
along with Emacs. First, start Emacs again and then enter the following
command to start the tutorial:

```
C-h t
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


### Unit 2.2: Emacs Init File

Let us now see how to customize Emacs with an initialization file.
Perform the following steps:

 1. Run Emacs again with the following command in the shell:

    ```sh
    emacs
    ```

 2. In Emacs, enter this command to create a new file at
    `~/.emacs`:

    ```
    C-x C-f ~/.emacs RET
    ```

 3. Then add the following lines of Emacs Lisp code:

    ```elisp
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq inhibit-startup-screen t)
    (load-theme 'wombat)
    ```

    The first three lines remove the menu bar, tool bar, and the scroll
    bar from Emacs running in GUI mode. This makes the window look
    minimal.

    The fourth line inhibits the startup screen with the `Welcome to GNU
    Emacs` message.

    The fifth line tells Emacs to load a beautiful dark color built-in
    theme known as `wombat`.

    If you are a beginner to Emacs, you might not want to disable the
    menu bar because it could be useful to access frequently used
    functions. Similarly, the startup screen has useful information for
    beginners, so you might want to gain more experience with Emacs
    before disabling it.

 4. If you want to check the other built-in themes, enter:

    ```
    M-x customize-themes RET
    ```

    A new window with a buffer named `*Custom Themes*` appear. In this
    buffer, select any theme you want to test. After you are done
    testing, you can close this new window with:

    ```
    C-x 0
    ```

 4. Quit Emacs now:

    ```
    C-x C-c
    ```

 5. Start Emacs again:

    ```sh
    emacs
    ```

    Emacs should now load with the theme and customization set in your
    `~/.emacs`.

    Emacs supports a number of initialization files such as `~/.emacs`,
    `~/.emacs.el`, `~/.emacs.d/init.el`, etc. Traditionally, Emacs users
    kept their initialization program in `~/.emacs`. We follow that
    tradition here. However, it may be more convenient to have all your
    Emacs in one directory, i.e., `~/.emacs.d`, so having the
    initialization configuration in `~/.emacs.d/init.el` may be a good
    idea too.


### Unit 2.3: Emacs Help

Emacs has a rich built-in help and beginners as well as experienced
users rely heavily on the built-in help. Here are some steps to get
started with the help system:

 1. Enter the following command to get help on how to use the help
    features:

    ```
    C-h C-h RET
    ```

 2. Enter the following command to search for commands whose name match
    the given `PATTERN`:

    ```
    C-h a PATTERN RET
    ```

    Note that `PATTERN` here is a placeholder for the actual pattern you
    want to search. Here is a concrete example that searches for
    commands with `buffer` in their names:

    ```
    C-h a buffer RET
    ```

 3. Enter the following command to get help for a key sequence:

    ```
    C-h k KEY-SEQUENCE
    ```

    Note that `KEY-SEQUENCE` here is a placeholder for the actual key
    sequence. Here are a few concrete examples:

    ```
    C-h k C-x C-f
    ```

    ```
    C-h k C-g
    ```

 4. Enter the following command to get help for a specific symbol.

    ```
    C-h o SYMBOL RET
    ```

    Note that `SYMBOL` here is a placeholder for the actual symbol. Here
    are a few concrete examples:

    ```
    C-h o load-theme RET
    ```

    ```
    C-h o menu-bar-mode RET
    ```


### Unit 2.4: More Emacs Commands

Here is a list of frequently used commands you may need in Emacs:

  - `C-x C-f FILENAME  RET`: Edit file `FILENAME`.
  - `C-g`: Signal quit condition. Also, cancel partial command.
  - `C-x C-s`: Save current buffer to file if modified.
  - `C-x C-c`: Quit Emacs.
  - `C-x C-b`: Show a list of exiting buffers.
  - `C-a`: Go to beginning of line.
  - `C-e`: Go to end of line.
  - `C-/` or `C-_` or `C-x u`: Undo.
  - `C-x b BUFFER RET`: Switch to a different buffer.
  - `C-x o`: Select next window.
  - `C-x k`: Kill buffer.
  - `C-x 0`: Kill window.
  - `C-x 4 0`: Kill buffer and window.
  - `C-h C-h`: Help for help.
  - `C-h a PATTERN RET`: Show commands that match pattern.
  - `C-h k KEY-SEQUENCE`: Show help for `KEY-SEQUENCE`.
  - `C-h o SYMBOL RET`: Show help for `SYMBOL`.


## Unit 3: SLIME, Paredit, Rainbow Delimiters

In this section, we will install three Emacs packages to turn Emacs into
an efficient development environment for Common Lisp:

  - Superior Lisp Interaction Mode for Emacs (SLIME): It is an Emacs
    mode that adds support for interacting with a running Common Lisp
    process for compilation, debugging, document lookup, etc. while
    developing Common Lisp applications.

  - Paredit: It helps in keeping parentheses balanced and in performing
    structured editing of S-expressions.

  - Rainbow Delimiters: It makes it easy to see matching parentheses by
    coloring parentheses at different depth levels with different
    colors.

Out of these three packages, only SLIME is essential. It is necessary to
install SLIME to get the full experience of Common Lisp programming with
its interactive evaluation and debuging features.

The other two packages, paredit and rainbow delimiters, are not so
essential, but they can be helpful in keeping track of the nested
parentheses.

The next section shows how to install these packages and enable them.


### Unit 3.1: Install Packages

 1. Start Emacs with this command:

    ```sh
    emacs
    ```

 2. Edit `~/.emacs` with this command:

    ```
    C-x C-f ~/.emacs RET
    ```

 3. Add the following Emacs Lisp code to the initialization file:

    ```elisp
    ; Enable installation of packages from MELPA.
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))

    ; Write customizations in ~/.emacs.d/custom.el instead of here.
    (setq custom-file (concat user-emacs-directory "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file))

    ; Install packages.
    (dolist (package '(slime paredit rainbow-delimiters))
      (unless (package-installed-p package)
        (package-install package)
        (require package)))

    ; Configure SLIME.
    (add-to-list 'exec-path "/usr/local/bin")
    (setq inferior-lisp-program "sbcl")

    ; Configure Paredit.
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
    (defun override-slime-del-key ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-del-key)

    ;Configure Rainbow Delimiters.
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
    ```

    There is a lot going on here but hopefully most of the code is
    self-explanatory. If you need a line-by-line explanation of what is
    going on here, see [Appendix A](appendix-a.md).

    By the way, this is all the Emacs Lisp code we are going to write to
    initialize Emacs. We are not going to work with Emacs Lisp anymore
    beyond this. You might, of course, want to write more Emacs Lisp
    code as you customize Emacs more and more to suit your needs as you
    learn more Emacs in future. If you have been following the steps in
    this document word-for-word until now, your Emacs initialization
    file should look like this: [.emacs](.emacs).

 4. Save the Emacs initialization file:

    ```
    C-x C-s
    ```

 5. Quit Emacs:

    ```
    C-x C-c
    ```

 6. Start Emacs again:

    ```
    emacs
    ```

    Emacs will take a while to start now because it will connect to
    MELPA and fetch the three packages we have mentioned in `~/.emacs`.

    After Emacs starts, check the content of `~/.emacs.d/elpa` directory
    which should now contain subdirectories for the three packages.

 7. Quit Emacs and start it again to ensure that it starts quickly the
    next time.

 6. Download the list of packages available in ELPA and MELPA.

    ```
    M-x package-refresh-contents RET
    ```


### Unit 3.2: Use SLIME

 1. Launch SLIME:

    ```
    M-x slime
    ```

    A new buffer named `*slime-repl sbcl*` should appear with the
    following prompt:

    ```
    CL-USER>
    ```

    This is a Read-Eval-Print-Loop (REPL) where you can evaluate Common
    Lisp expressions.


 2. Enter this in the REPL:

    ```lisp
    (+ 1 2)
    ```

    The following result should appear when you press <kbd>enter</kbd>:

    ```
    3
    ```

 3. Try one more example in the REPL:

    ```lisp
    (format t "hello, world~%")
    ```

    The following result shoud appear on pressing <kbd>enter</kbd>:

    ```
    hello, world
    NIL
    ```

 4. We will now see how to work on a Lisp source file and send
    expressions to the REPL for evaluation using SLIME commands without
    having to leave Emacs. First, create a buffer for a new file, for
    example:

    ```
    C-x C-f foo.lisp
    ```

 5. Now enter this Lisp code into the buffer for `foo.lisp`:

    ```lisp
    (+ 1 2)
    ```

 6. While the cursor is placed after the closing parenthesis (not on it,
    but after it), enter the following command:

    ```
    C-x C-e
    ```

    The result `3` should appear in a minibuffer at the bottom.


### Unit 3.3: Use Paredit

 1. Run Emacs:

    ```sh
    emacs
    ```

 2. Open a Common Lisp source file:

    ```
    C-x C-f foo.lisp
    ```

 3. Type the following code only:

    ```lisp
    (defun square (x
    ```

    At this point, Paredit should have inserted the two closing
    parentheses automatically. The code should look like this:

    ```lisp
    (defun square (x))
                    -
    ```

    The cursor should be situated just after the parameter `x`. The
    underbar shows where the cursor should be.

 4. Type the closing parentheses now. Yes, type it even if the closing
    parenthesis is already present. The cursor should now skip over the
    first closing parenthesis like this:

    ```lisp
    (defun square (x))
                     -
    ```

    Of course, there was no need to type the closing parenthesis because
    it was already present but typing it out to skip over it is more
    efficient than then moving over it with movement commands. This is,
    in fact, a very nifty feature of Paredit. We can enter code with the
    same keystrokes as we would without Paredit.

 5. Now press <code>enter</code> create a new line just before the last
    parenthesis. A newline is inserted like this:

    ```lisp
    (defun square (x)
      )
      -
    ```

 6. Now type only this:

    ```lisp
    (* x x
    ```

    Again, Paredit would have inserted the closing parenthesis
    automatically. The code should look like this now:

    ```lisp
    (defun square (x)
      (* x x))
            -
    ```

To see the various different modes in which Paredit has been enabled in
our Emacs initialization file, see [Appendix A](appendix-a.md).

Note: More content is coming up in this section. Check back in a week!

Note: Some portion of this section is based on [Lisp in Vim: Get Started
with Paredit][lisp-in-vim] which provides a step by step walkthrough of
how Paredit works in Vim. Since Vim's Paredit is based on Emacs'
Paredit, both work similarly to some extent.

[lisp-in-vim]: https://susam.in/blog/lisp-in-vim-with-slimv-or-vlime/#get-started-with-paredit


### Unit 3.4: Rainbow Delimiters

There is not much to learn about using Rainbow Delimiters. In the
previous sections, you must have seen that as you type nested
parentheses, each parenthesis is highlighted with a different color.
That is done by Rainbow Delimiters. It colors each parenthesis according
to its depth level. Rainbow Delimiters is a package that does what it
needs to without you being required to learn anything.


Forums
------

The following community forums are available for asking questions, and
talking to other members of our community:

- Reddit: [r/spxy](https://www.reddit.com/r/spxy/)
- Matrix: [#spxy:matrix.org](https://app.element.io/#/room/#spxy:matrix.org)
- Freenode: [#spxy](https://webchat.freenode.net/#spxy)
- Twitter: [@spxycc](https://twitter.com/spxycc)
- Mailing list: [groups.google.com/g/spxy](https://groups.google.com/g/spxy)

The Matrix and Freenode channels are bridged together, so if you join
either of them, you can see messages of both channels.


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
