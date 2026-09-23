# Start a matching run in a background R process

Serialises the inputs to a job directory and launches a worker against
them. Returns immediately; the caller polls with
\`.poll_matching_job()\`.

## Usage

``` r
.start_matching_job(
  user_df,
  col_name,
  backbone,
  min_similarity,
  include_authors,
  input_hash,
  rm_mode,
  checkpoint_file
)
```

## Arguments

- user_df, col_name, backbone, min_similarity, include_authors,
  input_hash, rm_mode:

  As for \`.run_matching_pipeline()\`.

- checkpoint_file:

  Character path for the checkpoint, computed by the caller so that both
  processes agree on it.

## Value

A job list: \`proc\` (the \`callr\` process), \`dir\`,
\`progress_file\`, \`result_file\`, \`cancel_file\`, \`started_at\`.

## Details

Note the backbone crosses the process boundary as a file, so it is
briefly held twice in memory and once on disk. That is the price of not
blocking the Shiny worker, and the backbone is the largest thing being
passed.
