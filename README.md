# UNfunny
UNfunny is a privacy-respecting frontend to iFunny that also removes image watermarks

iFunny? more like UNfunny!!!!

Currently only the images are supported because afaik thats the only reason anyone goes to ifunny.co. If you think I'm wrong, open an issue.

# Support
Join our [Matrix room](https://mto.vern.cc/#/#cobra-frontends:vern.cc) for support and other things related to UNfunny

# Instances
See instances.json

# Run your own instance
## Dependencies
This program is written in Guile Scheme.

You will need need `guile`, `guile-gnutls`, `guile-lib`, and `libvips` (for removing the watermark).

## Running
0. Install the dependencies
1. Clone the repository using `git clone --recurse-submodules https://git.vern.cc/cobra/UNfunny`
2. Run `guile -L . unfunny.scm`
3. Connect to http://localhost:8004 (or point your reverse proxy to it)
4. Profit

## Environment
`PATCHES_URL` - Link to any patches that were applied. Necessary if there are any. Do not set if there aren't.

The following are optional.

`PORT` - What port to run on (default `8004`).

`VIPS_PATH` - Path to `libvips.so` (default similar to below).

## Notes
When using a service manager (e.g. systemd), make sure the stop signal is set to SIGINT, or 2. This will prevent EADDRINUSE.
