# UNfunny

A privacy-respecting frontend to [iFunny](https://ifunny.co): browse memes, videos and GIFs without your browser ever talking to iFunny, without watermarks and without JavaScript.

This is a personal fork of [UNfunny](https://codeberg.org/cobra/UNfunny) by Skylar Astaroth. It is public to keep the code in sync and to leave a record of the changes; there is no list of public instances and no support channel. Like the original, it is licensed under the GNU AGPL v3 or later.

## What it does

- **Featured** (`/`, then `/page3`, `/page5`...): iFunny's featured memes.
- **Channels** (`/channels/<name>`), **tags** (`/tags/<tag>`) and **profiles** (`/user/<nick>`), each with a "Next page" link.
- **Posts** (`/picture/...`, `/meme/...`, `/video/...`, `/gif/...`): the picture without its watermark strip, or the video, with its author, date, smiles and comment count, tags, a download link and a link to the original.

Replacing `ifunny.co` with the instance's domain in any iFunny link opens the same page. For iFunny Brazil links, replace `br.ifunny.co` with the instance's domain and put `/br` in front of the path: `https://br.ifunny.co/picture/WEatpqh3B` becomes `https://<instance>/br/picture/WEatpqh3B`.

Every picture and video goes through the instance's own media proxy, which only fetches from iFunny's media hosts. Pages carry no scripts, no cookies and no third-party requests. `robots.txt` keeps crawlers out, since every page they fetched would become a request to iFunny.

## Changes from the original

- Pictures are fetched already cropped from iFunny's own image service instead of being decoded with libvips in-process. That removed a crash on any non-image file, a memory leak of about 3 MB per image and PNG re-encoding; libvips is no longer a dependency.
- Posts are read from the JSON iFunny embeds in its pages instead of a fixed path through their DOM.
- Videos and GIFs are supported. They never autoplay, so they work in browsers that block it without anyone changing a setting; GIFs, which iFunny only serves as MP4, play muted and loop.
- Featured, channels, tags and profiles can be browsed, with pages of up to 20 posts (see below).
- Post pages show the author, date, counters, tags and download links.
- Rendered pages are cached in memory (posts for an hour, feeds for ten minutes), and simultaneous visits to the same page share one request to iFunny.
- One slow answer from iFunny no longer freezes the instance: requests are served in parallel, with a 5 s connect timeout and a 15 s deadline for every request to iFunny.
- Failures become proper error pages (400, 404, 502, 503, 504) instead of blank 500s, and unknown routes answer 404.
- Redirects are followed safely, and the media proxy only follows them within iFunny's media hosts.
- Pages render in standards mode, with a layout that fits phones.
- The "Source Code" link points to this fork (see `SOURCE_URL`).
- The Docker image is built from local source, runs as an unprivileged user and stops cleanly.

The commit history explains each change in detail.

## About "Next page"

iFunny's pages only hold a few posts: 5 for a tag, 6 for a channel, 10 for Featured, 1 to 3 for a profile. Its web client loads more as you scroll, from a JSON API that answers only with the anonymous session iFunny gives every visitor (the cookies any of its pages sets).

UNfunny keeps that session from the pages it fetches anyway and, for each page, asks the API for up to three more batches until it has about 20 posts. A click costs at most four requests to iFunny, about what iFunny's own page asks for while you scroll through the same posts.

This may stop working at any time. If the API refuses requests, links that depend on it disappear for an hour and everything else keeps working.

## Running

### Dependencies

Guile 3.0 with `guile-gnutls`, `guile-lib` and `guile-json`. On Debian or Ubuntu:

    sudo apt install guile-3.0 guile-library guile-gnutls guile-json

### Bare metal

1. Install the dependencies.
2. `git clone https://github.com/darlopvil/unfunny-fork.git && cd unfunny-fork`
3. `guile unfunny.scm`
4. Open http://localhost:8004, or point your reverse proxy at it.

### Docker

1. Clone the repository and `cd` into it.
2. `cp docker-compose.yml.example docker-compose.yml` and adapt it to your setup. `docker-compose.yml` is ignored by git, so your setup stays out of the repository.
3. `docker compose up -d --build`

Run `docker compose up -d --build` again after pulling changes.

### Environment

| Variable | Default | Purpose |
| --- | --- | --- |
| `PORT` | `8004` | Port to listen on. |
| `SOURCE_URL` | this repository | Where the "Source Code" link in the footer points. If you run a modified copy, the AGPL requires you to offer its source to your users, so point this at it. |
| `PATCHES_URL` | unset | Link to patches applied on top of the source, shown in the footer. Set it only if there are any. |

## License

UNfunny was written by Skylar Astaroth. This fork keeps the original copyright notices and adds its own to the files it changes or adds.

UNfunny is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. See [LICENSE](LICENSE).
