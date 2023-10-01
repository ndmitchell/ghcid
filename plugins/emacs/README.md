
# Table of Contents

1.  [Features](#orgfe72dbf)
2.  [installation](#org7f171c2)
    1.  [doom emacs](#org878185b)
    2.  [use-package](#orgb73895b)
3.  [Configuration](#org5de9a04)
4.  [Jump to error](#org25a2f61)

This is a ghcid minor mode support for emacs. It uses a terminal with compilation-mode.


<a id="orgfe72dbf"></a>

# Features

This minor mode will provide following features when activated:

-   start ghcid automatically once activated
-   detect the project root directory based on open buffer file
-   check project build tools and construct the ghcid command accordingly
-   jump to error position
-   customize the ghcid repl command, test command, setup command or lint command


<a id="org7f171c2"></a>

# installation


<a id="org878185b"></a>

## doom emacs

With doom emacs, add following to `packages.el`:

    (package! ghcid
      :recipe (:host github :repo "hughjfchen/ghcid-mode"))

and following to the `config.el`:

    (use-package! ghcid
      :config (load! ghcid))


<a id="orgb73895b"></a>

## use-package

For vallina emacs, recommand use `use-package`:
install it:

    (straight-use-package '(ghcid-mode :host github :repo "hughjfchen/ghcid-mode"
                                    :files (:defaults "*.el")))

and then config to activate it when haskell-mode is active:

    (use-package haskell-mode
        :init
         (add-hook 'haskell-mode-hook #'ghcid-mode))


<a id="org5de9a04"></a>

# Configuration

If ghcid-mode not able to start ghcid with correct command, you can config it using a `.dir-locals.el` file as so:

    ((haskell-mode . ((ghcid-repl-command-line . ("cabal" "new-repl" "hsprjup:lib:hsprjup" "\\-f" "ghcidlibwithtest"))
             (ghcid-test-command-line . "TestMain.main")
             (ghcid-setup-command-line . ":load test/Spec.hs"))))

Following table lists the customization variables:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">name</th>
<th scope="col" class="org-left">type</th>
<th scope="col" class="org-left">meaning</th>
<th scope="col" class="org-left">remarks</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">ghcid-project-root</td>
<td class="org-left">string</td>
<td class="org-left">the project root directory</td>
<td class="org-left">usually no need to set it, ghcid-mode should detect it automatically</td>
</tr>


<tr>
<td class="org-left">ghcid-target</td>
<td class="org-left">string</td>
<td class="org-left">the ghci repl target, could be a lib, an exe or a test</td>
<td class="org-left">For multi target you could set</td>
</tr>


<tr>
<td class="org-left">ghcid-repl-command-line</td>
<td class="org-left">a list with string</td>
<td class="org-left">the ghcid command line to start the ghci</td>
<td class="org-left">the whole command to start the ghci</td>
</tr>


<tr>
<td class="org-left">ghcid-test-command-line</td>
<td class="org-left">string</td>
<td class="org-left">the ghcid test command</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">ghcid-setup-command-line</td>
<td class="org-left">string</td>
<td class="org-left">the ghcid setup command</td>
<td class="org-left">&#xa0;</td>
</tr>


<tr>
<td class="org-left">ghcid-lint-command-line</td>
<td class="org-left">string</td>
<td class="org-left">the ghcid lint command</td>
<td class="org-left">&#xa0;</td>
</tr>
</tbody>
</table>


<a id="org25a2f61"></a>

# Jump to error

This mode uses `compilation-mode` within a terminal, so if there&rsquo;re some build errors, you can use the `flycheck`
keys(usually `]+q` and `[+q`) to jump to the errors.

