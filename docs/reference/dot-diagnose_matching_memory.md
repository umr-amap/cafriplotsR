# Recognise a failure that was really the machine running out of memory

One condition wears three different faces, and none of them says "out of
memory" in words a user would recognise:

## Usage

``` r
.diagnose_matching_memory(logs = "", status = NA_integer_)
```

## Arguments

- logs:

  Character scalar. Whatever output the failure produced — a worker's
  combined logs, or \`conditionMessage()\` from an in-process run.

- status:

  Integer exit status of the worker, or NA when there was none.

## Value

A character scalar to show the user, or NULL when this was not a memory
failure and the caller should report what it has.

## Details

\* Windows refuses to map a DLL once the commit limit (RAM plus
pagefile) is reached, and reports \`LoadLibrary failure: The paging file
is too small\`. The DLL it names is whichever one the loader reached
first and is not the cause. \* R reports a failed allocation. \*\*The
size in that message is not informative\*\*: at the wall it names
whatever small object came next, which is why users see "cannot allocate
vector of size 2.6 Mb" and reasonably conclude their file is not the
problem. \* A container's OOM killer leaves exit status 137 and no
message at all.

Left raw, the first two read as unrelated bugs. All three get the same
explanation, and above all the instruction to restart R: once a session
has hit this, every retry inside it fails the same way, including the
\`cafri.async_matching = FALSE\` fallback.
