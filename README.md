## DISCLAIMER
**This is very early stage software; it is a proof of concept that is not meant for production use!**

### Shortcomings

We currently add `Connection: close` to the request and use lazy IO to read the
body.  This is pretty fragile and we want to use proper body parsing in the
future.  Also, the way we deal with line endings won't work on Windows.
