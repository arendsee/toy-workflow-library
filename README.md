# Toyflow

This is a sandbox repository for figuring out a good system for doing
computations in Haskell. Each step in the workflow needs to handle potential
errors (which toggle failed states), warnings (which don't but need to be shown
to the user), caching and cache invalidation, result generation, summary
chaining, etc etc. The steps need to fold together into a final result, which
contains references to outputs (on success) or an error report (on failure).
