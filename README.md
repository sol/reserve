## Shortcomings

We currently add `Connection: close` to the request and use lazy IO to read the
body.  This is pretty fragile and we want to use proper body parsing in the
future.  Also, the way we deal with line endings won't work on Windows.
