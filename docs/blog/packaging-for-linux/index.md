---
title: "I hate packaging my software for Linux"
date: 2026-08-11
description: "Nineteen distribution channels later: what it actually costs to give a terminal editor away for free on Linux, and why the next step is a static musl binary that updates itself."
outline: false
---

# I hate packaging my software for Linux

All I wanted was for [Fresh](https://github.com/sinelaw/fresh) to be easy to install, for everybody, everywhere - how hard can it be?!

Windows and macOS aren't so bad, because they're relatively uniform. For Windows, I
use winget - ugly, but works (once I got past the initial headache). For macOS, I'm
using homebrew for now, which isn't ideal, but it wasn't hard to set up. I'll probably move to a more native
signed mac app once I take the time to package it correctly. Both solutions work
ok for modern Windows and macOS users.

*Linux is a different story*. I started by releasing Fresh as an npm package. This
felt ugly and [annoyed some people on
hn](https://news.ycombinator.com/item?id=46137819) but also has real problems:

- **Security** — npmjs has had multiple breaches and I want to sleep better at
  night.
- **Not universal** — many users don't have npm installed, why would they
  install npm just to install Fresh?
- **Weird installer** — the npm install flow, it's actually a script that you
  get from npmjs which goes to github and downloads the correct binary artifact
  for your machine. You have to do this every time you want to update.

So I listened to the annoyed people's feedback, and (with the help of several
gracious contributors) we went ahead and created packages for EVERYTHING:

- rust's cargo on crates.io (and also cargo-binstall)
- un-distros like AppImage and Flatpak
- deb
- rpm
- AUR (Arch Linux), two variants — source build and pre-built binary `-bin`
  package
- nix
- mise
- homebrew for linux (wtf?)
- npm / npx
- Terra
- Gentoo GURU

Plus I'm releasing pre-built binaries as a tarball. You can imagine how fragile
all of this can be. After each release I wonder if I'll hit some problem or
another with one of the many channels.

## "Why not use nix?!"

Because many people DON'T HAVE NIX installed and don't want to install it just
to use my app. I support nix as a method but it doesn't work for everybody.

## "Just use Flatpak!"

I do release a Flatpak but I'm actually working against it. It's designed for
self-contained sandboxed desktop GUI applications and I'm releasing a
terminal-based TUI that allows you to rampage around your machine and network.
I'm passing some horrible "bad practice" flags to get it to work. Flatpak
sandboxing is not a good fit.

## "Just use AppImage!"

I do release an AppImage, but it's extremely slow to run because of the squashfs FUSE-mount on-demand, which makes bringup time unacceptably slow. I want my program to start *instantly*, as quickly as technically possible. To make bringup times reasonable, my installer script bakes in a horrible hack to extract the squashfs contents somewhere and drop the AppImage. You can still just run the AppImage if you prefer, but it'll be annoyingly slow to start. And in both cases it requires FUSE to be installed. Why does my app need FUSE?!

Also, since my binary requires some minimal libc version, it isn't actually
universally portable (so for a 2-3 year old distro it might not work), hence I
might as well use static linking and also just drop AppImage. For some reason I imagined that using AppImage alone solves this portability problem.

Lastly, for various reasons, many people have a bad view of AppImage and Flatpak (and Snap) and they would just refuse to use it. I lose.

## Debian family pain

As explained in [this other hn
discussion](https://news.ycombinator.com/item?id=48347180) I hadn't gotten
around to pushing my `.deb` as a Debian (and Ubuntu) official package, because it requires all the (many) rust dependencies to also be Debian packages. I
understand the merit of this approach — fully reproducible and self-contained
builds for any package, also reduces supply chain hell, etc. — but it's a lot of work to do. And how will I ever keep up — every one of my direct dependencies will need to be re-updated on Debian on every security issue etc. I don't have time for that. Maybe I should just vendor all my deps as sources into my deb source package? I don't know if the Debian maintainers will like that.

Another fun anecdote is that I do want to support older machines running e.g.
older Ubuntu (and I guess any older distro) — but these have older libc, meaning my build for newer ubuntu can't be installed on the older ones as they fail on link error at binary load time. So I'd have to build in an old Ubuntu container image and inherit its unpatched packages into my build — or drop those users entirely. Neither option holds up.

## No automatic updates for .deb / .rpm

Because it's such a headache, I didn't get my package `.deb` (or `.rpm`) accepted into the official sources. Therefore, people who install these packages don't get automatic updates when they run their system's native package update mechanism, `apt-get upgrade` or equivalent. It would've been nice if there was a quick serverless solution where you could just say a URL to look for newer versions of a single package, and both apt and dnf would remember that and update a package. 

I'm well aware that the "correct" thing to do here is to get my package into the official channels. I opened a bugzilla thing for Fedora and someone started a whatever thing for Ubuntu and there are votes etc, but I just don't have the time and energy to push these requests through. I just want to GIVE AWAY MY SOFTWARE. I guess my point is: since people use all kinds of distros, the compounded effort of doing this is very high.

## Mise worked but then it didn't

Turns out some people use mise as their local (user-level) package manager. Someone got it to work for Fresh, which is great! But one day it stopped working with no change on my end. Turns out GitHub rotated the certificate of its build attestation system, and mise had pinned an outdated trust root instead of fetching the current one. I didn't know anything was broken until someone complained. I ended up [fixing it myself](https://github.com/jdx/mise/pull/10677) but why am I
spending my time on that kind of problem? Do I even know what I'm doing? I mean mise is cool and I also use it for some projects but as an author of some software tool - there are just too many of these things to think about.

## Arch Linux AUR

I personally use Arch AUR and also provide Fresh through this channel (both as a source and pre-built -bin package), but lately it's been blocked for new package releases. AUR is in read-only mode because of some security breaches, so I already missed a couple of version releases. So far I have no idea when AUR will be back. As a mitigation I've reached out to a maintainer to get Fresh into the Arch `extra` repo instead of AUR, so maybe that will be the solution.

The bottom line of all this ranting is that distro-specific channels are not a good solution. As the number of distros and variants increase it's becoming near impossible to release even a minor package.

## Next steps: focus on the statically linked musl, self-updating binary

Do I even need a package manager?

I already build a musl binary as part of the release. The next version will include a new built-in self-updating mechanism that users can trigger on demand. This will be the main release channel for Linux and hopefully the only one I'll need to support going forward. By making it the default, I'm taking a risk because who knows what other surprises it will bring — some Linux portability issue that will break my statically linked binary for some people. They can still build from source or use one of the package files I'm still releasing, for now.

Basically I'm implementing my own little package manager. Will it work for
everybody? Hopefully! Fresh is ~12MB to download and extracts to ~35MB so it
should be a reasonable experience overall. I'm constantly working on reducing
that final binary size too.

Is there anything else I can do?
