## How to contribute to this project

#### **Did you find a bug?**

* **Ensure the bug was not already reported** by searching on GitHub
  under the Issues page.

* If you're unable to find an open issue addressing the problem,
  open a new issue. Be sure to
  include a **title and clear description**, as much relevant information
  as possible, and labels.

#### **Did you write a patch that fixes a bug?**

* Make sure there is an open issue that describes the bug.

* Open a new GitHub Pull Request with the patch. Make sure your branch
  follows the branch conventions mentioned below.

* Ensure the PR description clearly describes the problem and solution.
  Include the relevant issue number by writing `Fixes #<issue number>` on
  the last line if applicable. Github will underline `Fixes` to indicate
  that the corresponding issue will automatically be closed when the PR
  gets merged.

* Before submitting, please make sure you have followed the coding and
  commit message conventions mentioned below, that the code compiles
  without any new errors or warnings.

#### **Did you fix whitespace, format code, or make a purely cosmetic patch?**

Changes that are cosmetic in nature and do not add anything substantial
to the stability, functionality, or testability of the software will generally
not be accepted for various reasons, including polluting the git history
and making backporting fixes harder.

#### **Do you intend to add a new feature or change an existing one?**

* Open an issue on GitHub to collect positive feedback about the change.
  Feature requests may be denied if they fall outside the scope of the project.

## Conventions

#### **Branches**

* Make sure that your branch name starts with the issue number and contains
  only a few words in lower case. For example: `1-performance`.

* Make sure that each commit contains one logical change. Do not add
  commits that only fix whitespace, formatting, or typo's. Instead amend
  the previous commit or perform interactive rebasing.

* Do not add `Merge branch...` commits. If you work on the branch from
  multiple computers or with multiple users, you should use `git pull --rebase`
  before you push your locally created commits.

* Your commits should tell a story how you fixed a bug or implemented a
  feature. If you have made a commit and want to fix some small mistakes
  in it, you can use `git commit --amend`. If you need to clean up your
  commits history by squashing or removing commits, you should use
  [interactive rebasing](https://www.atlassian.com/git/tutorials/merging-vs-rebasing).
  You can rebase using `git rebase -i master` or `git rebase -i HEAD~5`
  to fix your last 5 commits for example.

* Do not try to modify parts of the code that do not fix/implement your
  bug/feature. This will make it more difficult to do code reviews. Follow
  the coding style of the code that you want to modify, even if it deviates
  from the coding convention in this document.

#### **Commit messages**

* Prefix the first line of your commit message with (for example) `foo: `
  if applicable. See previous commits to see whether the project regularly
  uses prefixes in commit messages.

* Make sure each line is at most 72 characters. If necessary, leave the
  second line blank and write a more detailed explanation starting on the
  third line. Wrap the message manually to 72 characters so that you don't
  have to manually scroll in order to read them.

* Do not end the first line with a period.

* Use the imperative mood. Use words like "Fix" and "Add" instead of
  "Fixed" and "Added".

* See [this blog post](https://chris.beams.io/posts/git-commit/) on how
  to write good commit messages.

* Add a `Signed-off-by` with `git commit -s` to sign the
  [Developer Certificate of Origin](#developer-certificate-of-origin) below.

#### **Coding style**

* Make sure your changes do not cause the compiler to fail with any new
  errors or warnings.

* Avoid trailing whitespace. You can check this with `git diff --check`.

* Use 3 space indentation, 2 spaces when continueing a statement on the
  next line, and UNIX line endings.

* If you add a subprogram with many parameters, put the parameters on the
  next line and try to align the `:`.
  If the subprogram header is less than about 90 characters, then you may
  keep it all on one line.

* If you add aspects, indent `with` by 2 spaces. If the parameters
  are on separate lines then indent by zero spaces.

* Put the `is` keyword on a separate line if the parameters are on separate
  lines *and* the declarative part is not empty. Otherwise place `is` after
  the parameters or return type.

* In general, try to follow the coding style of the surrounding code.

#### Licensing

* Each file should contain an `SPDX-License-Identifier` tag and a license
  header. Contributions created in whole by you should be licensed under
  the main license of the project, as specified in the [README][url-readme].

## Developer Certificate of Origin

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I have the
right to submit it under the open source license indicated in the file; or

(b) The contribution is based upon previous work that, to the best of my
knowledge, is covered under an appropriate open source license and I have
the right under that license to submit that work with modifications,
whether created in whole or in part by me, under the same open source
license (unless I am permitted to submit under a different license), as
indicated in the file; or

(c) The contribution was provided directly to me by some other person who
certified (a), (b) or (c) and I have not modified it.

(d) I understand and agree that this project and the contribution are
public and that a record of the contribution (including all personal
information I submit with it, including my sign-off) is maintained
indefinitely and may be redistributed consistent with this project or
the open source license(s) involved.

  [url-readme]: /README.md
