Appendix A: Minimal Emacs Init File to Begin Learning Common Lisp
=================================================================

This document explains the minimal [.emacs](.emacs) file provided by
this repository. It is a minimal Emacs initialization file useful to
begin learning Common Lisp.

Note: This is an appendix to the main [README.md](README.md) of this
project. To learn more about how to install Emacs, SBCL, Paredit, and
Rainbow Delimiters as well as how to use them, see the
[README.md](README.md).

Each line of our minimal [.emacs](.emacs) file is explained below.

  - Hide the menu bar:

    ```elisp
    (menu-bar-mode -1)
    ```

    If you are running Emacs with a GUI window, then by default it
    starts with a menu bar, tool bar, and scroll bar. Experienced
    users use Emacs completely through the keyboard via the various
    key-bindings for various operations, so many of them hide these
    additional bars to make the Emacs window look clean and minimal. 

    If you are a beginner to Emacs, you might find the menu bar helpful
    initially, so you might not want this line in your Emacs
    initialization file. In that case, remove this line or just comment
    it out by inserting a semicolon (i.e., `;`) before the opening
    parentheses.


  - Hide the tool bar:

    ```elisp
    (tool-bar-mode -1)
    ```

  - Hide the scroll bar:

    ```elisp
    (scroll-bar-mode -1)
    ```

  - Inhibit the startup screen with the `Welcome to GNU Emacs` message
    from appearing:

    ```elisp
    (setq inhibit-startup-screen t)
    ```

    If you are a beginner to Emacs, you might find the startup screen
    helpful, so you might not want this line in your Emacs
    initialization file. In that case, remove this line or just comment
    it out by inserting a semicolon (i.e., `;`) before the opening
    parentheses.

  - Display line numbers in all buffers:

    ```elisp
    (global-display-line-numbers-mode)
    ```

  - Load a beautiful dark color theme known as `wombat`:

    ```elisp
    (load-theme 'wombat)
    ```

    If you want to check the other built-in themes, enter
    `M-x customize-themes RET`. A new window with a buffer named
    `*Custom Themes*` appear. In this buffer, select any theme you want
    to test. After you are done testing, you can close this new window
    with `C-x 0`.

  - This is necessary for defining the `package-archives` list we will
    use in the next point.

    ```elisp
    (package-initialize)
    ```

  - Add MELPA to the list of archives to fetch packages from:

    ```elisp
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    ```

    By default only ELPA is in the list of archives to fetch from. The
    above snippet adds MELPA too to the list. If you are curious to see
    what the original value of `package-archives` was and what it is now
    due to the above line, enter `C-h o package-archives RET`.

  - Download package descriptions from package archives only if they
    have not been downloaded before:

    ```elisp
    (unless package-archive-contents
      (package-refresh-contents))
    ```

    The first line checks whether package descriptions from package
    archives from archives have been fetched. See
    the `~/.emacs.d/elpa/archives` directory for archive contents in
    case you are curious. If the archive contents have not been fetched
    then the next line fetches them. Thus the second line executes only
    when the Emacs initialization is loaded for the first time.

  - When we install packages using `package-install` (coming up in the
    next point), a few customizations are written automatically into the
    Emacs initialization file (`~/.emacs` in our case). This has the
    rather undesirable effect of our carefully handcrafted `~/.emacs`
    being meddled by `package-install`. To be precise, it is the
    `custom` package invoked by `package-install` that intrudes into our
    Emacs initialization file. To prevent that, we ask `custom` to write
    the customizations to a separate file at `~/.emacs.d/custom.el` with
    the following code:

    ```elisp
    (setq custom-file (concat user-emacs-directory "custom.el"))
    ```

  - Install SLIME, Paredit, and Rainbow Delimiters only if they are not
    installed already:

    ```elisp
    (dolist (package '(slime paredit rainbow-delimiters))
      (unless (package-installed-p package)
        (package-install package)))
    ```

    This loops iterates over each package name in a list of three
    package names. For each package, it checks whether the package is
    installed with the `package-installed-p` function. If it is not
    installed, then it is installed with the `package-install` function.
    Thus, the first time Emacs starts with this initialization file, it
    takes a while to install the three packages we need. However, once
    the packages are installed, and Emacs is started again later, it
    starts instantly because the code above takes care to not attempt
    installing packages that are already installed.

  - On macOS, when we install SBCL using `brew install sbcl` (as
    explained in [README.md](README.md)), the compiler binary executable
    is written to `/usr/local/bin/sbcl`. The path `/usr/local/bin` is
    generally available in the shell's `$PATH` environment variable, so
    when Emacs is launched from the shell, its `exec-path` variable
    contains `/usr/local/bin`. As a result, it can find `sbcl` even if
    we just tell SLIME that our Lisp program is `sbcl` (see next point)
    without its complete path.

    However, when Emacs is launched from the desktop (say, from macOS
    Dock), it does not have `/usr/local/bin` in its `exec-path`, so if
    we point SLIME to only `sbcl` without its complete path, then it
    fails to start with this error: `Searching for program: No such file
    or directory, sbcl`. The following line of code works around this
    issue by adding `/usr/local/bin` to the `exec-path` variable:

    ```elisp
    (add-to-list 'exec-path "/usr/local/bin")
    ```

    Now there are several other ways to resolve this issue. A popular
    way is to specify the absolute path of SBCL in the next point. Yet
    another way is to configure macOS desktop such that when a program
    is launched from GUI, it contains `/usr/local/bin` in its `PATH`.

    The workaround shown above is recommended in this document for
    two reasons. Firstly, we don't want to hard-code absolute path of
    SBCL in the Emacs initialization file, so that the same
    initilization file can work well on other systems where the location
    of SBCL may be different. Secondly, we want to keep the workaround
    minimally invasive, so that we don't have to go around meddling with
    the desktop settings only for the sake of Emacs.

  - Specify the program to be invoked for loading and executing SLIME:

    ```elisp
    (setq inferior-lisp-program "sbcl")
    ```

  - Enable Paredit while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
    ```

    Paredit helps in keeping parentheses balanced and in performing
    structured editing of S-expressions. While we configure it to be
    used for Common Lisp programming, we might as well configure it for
    editing Emacs Lisp code too. Then the experience of editing Emacs
    Lisp code and that of editing Common Lisp code will be consistent
    with each other.

    To test that Paredit is enabled for editing Emacs Lisp code, open a
    new Emacs Lisp file, say, `foo.el`. Then type `(`. Paredit should
    automatically insert the corresponding `)`.

  - Enable Paredit in eval-expression minibuffer:

    ```elisp
    (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-:` to bring up the eval-expression minbuffer
    and type `(`. Paredit should automatically insert the corresponding
    `)`.

  - Enable Paredit while interactively evaluating Emacs Lisp expressions
    in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer comes
    up, type `(`. Paredit should automatically insert the corresponding `)`.

  - Enable Paredit while editing Common Lisp code:

    ```elisp
    (add-hook 'lisp-mode-hook 'enable-paredit-mode)
    ```

    To test this, open a new Common Lisp source file, say, `foo.lisp`.
    Then type `(`. Paredit should automatically insert the corresponding
    `)`.

  - Enable Paredit in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
    ```

    To test this, first open a non-Lisp file, say, `foo.txt`. Now type
    `(`. Note that no corresponding `)` is inserted because we are not
    in Lisp interaction mode yet. Delete `(`. Then start Lisp
    interaction mode with the command `M-x lisp-interaction-mode RET`.
    Type `(` again. Paredit should now automatically insert the
    corresponding `)`.

  - Enable Paredit in SLIME REPL:

    ```elisp
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
    ```

    To test this, start SLIME with `M-x slime RET`. Then type `(` in
    SLIME REPL. Paredit should automatically insert the corresponding
    `)`.

  - When we press <kbd>backspace</kbd>/<kbd>delete</kbd> key, to delete
    a parenthesis in the SLIME REPL, Paredit fails to keep the
    parentheses balanced because SLIME interferes with Paredit by
    grabbing the delete key. To fix this issue, use the following code:

    ```elisp
    (defun override-slime-del-key ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
     ```

     To test this, start SLIME with `M-x slime RET`. Then type `(+ 1 (+
     2 (+ 3 4)))`. Even though, the closing parentheses `)))` will be
     automatically inserted, type them out to advance the cursor to the
     end of the line. When you type `)` even if it is already present,
     Paredit just skips over the already present `)`. Once you are at
     the end of the line, press <kbd>backspace</kbd>/<kbd>delete</kbd>
     multiple times. Paredit will keep the parentheses balanced at all
     times.

  - Enable Rainbow Delimiters while editing Emacs Lisp code:

    ```elisp
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    Rainbow Delimiters color nested parentheses with different colors
    according to the depth level of each parenthesis.

    To test this open a new Emacs Lisp file, say, `foo.el`. Then type
    `((((`. Rainbow Delimiters should color each parenthesis
    differently.

  <!-- 
    The next point does not work. See
    https://github.com/Fanael/rainbow-delimiters/issues/57 for details.
  -->

  <!-- 
  - Enable Rainbow Delimiters in eval-expression minibuffer:

    ```elisp
    (add-hook 'eval-expression-minibuffer-setup-hook 'rainbow-delimiters-mode)
    ```

    To test this, enter `M-:` to bring up the eval-expression minbuffer
    and type `((((`. `((((`. Rainbow Delimiters should color each
    parenthesis differently.
  -->

  - Enable Rainbow Delimiters while interactively evaluating Emacs Lisp expressions
    in inferior-emacs-lisp-mode (IELM):

    ```elisp
    (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, enter `M-x ielm RET`. When the `*ielm*` buffer comes
    up, type `((((`. Rainbow Delimiters should color each parenthesis
    differently.

  - Enable Rainbow Delimiters while editing Common Lisp code:

    ```elisp
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, open a new Common Lisp source file, say, `foo.lisp`.
    Then type `((((`. Rainbow Delimiters should color each parenthsis
    differently.

  - Enable Rainbow Delimiters in Lisp interaction mode:

    ```elisp
    (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, first open a non-Lisp file, say, `foo.txt`. Now type
    `((((`. Then start Lisp interaction mode with the command `M-x
    lisp-interaction-mode RET`. Rainbow Delimiters should now color each
    parenthesis differently.

  - Enable Rainbow Delimiters in SLIME REPL:

    ```elisp
    (add-hook 'slime-repl-mode-hook 'rainbow-delimiters-mode)
    ```

    To test this, start SLIME with `M-x slime RET`. Then type `(` in
    SLIME REPL. Rainbow Delimiters should automatically insert the corresponding
    `)`.

You may have noticed that we did not enable Rainbow Delimiters for
eval-expression. That is because it does not work as expected as of
Dec 2020. See https://github.com/Fanael/rainbow-delimiters/issues/57 for
more details.
