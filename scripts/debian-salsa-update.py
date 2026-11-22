#!/usr/bin/env python3
"""Update the Salsa repo (sinelaw/fresh-editor) to a Fresh release.

Usage: debian-salsa-update.py [--repo DIR] prepare VERSION [REVISION]
       debian-salsa-update.py [--repo DIR] release

prepare  Import VERSION (repacked if debian/watch has a Repacksuffix), set the
         changelog version, commit, build and lintian. Salsa's debian/ is
         otherwise left alone. Pushes nothing.
release  Sign, push, open a self-merging Salsa MR into debian/latest, wait
         for CI, upload to mentors and push the debian/<version> tag.
         Unlock the gpg key first.

DIR defaults to $SALSA_REPO. Debian tools run in docker.
"""
import json
import os
import re
import shutil
import subprocess
import sys
import time
import urllib.request
from email.utils import formatdate
from pathlib import Path

IMAGE = "fresh-deb-tools"
DOCKERFILE = """FROM debian:unstable
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \\
    git git-buildpackage pristine-tar devscripts dput lintian dpkg-dev debhelper gnupg \\
    ca-certificates curl xz-utils libwww-perl liblwp-protocol-https-perl dh-cargo python3-debian \\
    && rm -rf /var/lib/apt/lists/*
"""
GPG_SETUP = ("mkdir -m700 /tmp/.gnupg && cp /gnupg-host/pubring.kbx /gnupg-host/trustdb.gpg /tmp/.gnupg/"
             " && ln -s /run/host-gpg-agent /tmp/.gnupg/S.gpg-agent && echo no-autostart > /tmp/.gnupg/gpg.conf")
DPUT_CF = ("[mentors]\nfqdn = mentors.debian.net\nincoming = /upload\nmethod = https\n"
           "allow_unsigned_uploads = 0\nallowed_distributions = .*\n")
API = "https://salsa.debian.org/api/v4/projects/sinelaw%2Ffresh-editor"
REPO = Path()
NAME = EMAIL = ""  # debian/control Maintainer


def say(msg):
    print(f"==> {msg}", flush=True)


def run(*cmd, **kw):
    return subprocess.run(cmd, check=True, text=True, **kw)


def git(*args, **kw):
    return run("git", "-C", REPO, "-c", f"user.name={NAME}", "-c", f"user.email={EMAIL}", "-c", "commit.gpgsign=false",
               "-c", "tag.gpgsign=false", "-c", "gpg.format=openpgp", *args, **kw)


def out(*args):
    return git(*args, capture_output=True).stdout.strip()


def deb(*cmd):
    if subprocess.run(["docker", "image", "inspect", IMAGE], capture_output=True).returncode:
        run("docker", "build", "-t", IMAGE, "-", input=DOCKERFILE)
    run("gpgconf", "--launch", "gpg-agent")
    sock = run("gpgconf", "--list-dirs", "agent-socket", capture_output=True).stdout.strip()
    gnupg = os.environ.get("GNUPGHOME", Path.home() / ".gnupg")
    env = [f"{k}={NAME}" for k in ("GIT_AUTHOR_NAME", "GIT_COMMITTER_NAME", "DEBFULLNAME")]
    env += [f"{k}={EMAIL}" for k in ("GIT_AUTHOR_EMAIL", "GIT_COMMITTER_EMAIL", "DEBEMAIL")]
    run("docker", "run", "--rm", "-i", "--user", f"{os.getuid()}:{os.getgid()}", "-e", "HOME=/tmp",
        *(a for e in env for a in ("-e", e)),
        "-v", f"{gnupg}:/gnupg-host:ro", "-v", f"{sock}:/run/host-gpg-agent",
        "-v", f"{REPO.parent}:/w", "-w", f"/w/{REPO.name}",
        IMAGE, "sh", "-c", GPG_SETUP + ' && exec "$@"', "sh", *cmd)


def version():
    return re.match(r"\S+ \((\S+)\)", (REPO / "debian/changelog").read_text())[1]


def changes():
    return f"../build-area/fresh-editor_{version()}_source.changes"


def require_clean():
    if out("rev-parse", "--abbrev-ref", "HEAD") != "debian/latest" or out("status", "--porcelain"):
        sys.exit(f"error: {REPO} must be on debian/latest with a clean working tree")


def build():
    say(f"building and checking source package {version()}")
    shutil.rmtree(REPO.parent / "build-area" / f"fresh-editor-{version().rsplit('-', 1)[0]}", ignore_errors=True)
    deb("gbp", "buildpackage", "--git-pristine-tar", "--git-export-dir=../build-area",
        "--git-builder=dpkg-buildpackage -S -sa -d -nc -us -uc")
    deb("lintian", "--info", "--display-info", "--pedantic", "--fail-on", "error,warning", changes())


def prepare(release, revision="1"):
    require_clean()
    # A repacked orig (Files-Excluded) gets the watch file's suffix, e.g. 0.5.9+ds.
    suffix = re.search(r"^Repacksuffix:\s*(\S+)", (REPO / "debian/watch").read_text(), re.M | re.I)
    upstream = release + (suffix[1] if suffix else "")
    if not out("tag", "-l", f"upstream/{upstream}"):
        say(f"importing upstream {upstream}")
        deb("uscan", "--download-version", release, "--destdir", "..")
        origs = list(REPO.parent.glob(f"fresh-editor_{upstream}.orig.tar.*"))
        if len(origs) != 1:
            sys.exit(f"error: expected one orig tarball for {upstream} in {REPO.parent}, found {origs}")
        deb("gbp", "import-orig", "--no-interactive", "--pristine-tar", "--no-sign-tags",
            f"--upstream-version={upstream}", f"../{origs[0].name}")

    changelog = REPO / "debian/changelog"
    def set_changelog(date):
        text = re.sub(r"\A(\S+) \([^)]*\)", rf"\1 ({upstream}-{revision})", changelog.read_text())
        changelog.write_text(re.sub(r"^( -- .*>  ).*$", lambda m: m[1] + date, text, count=1, flags=re.M))
        git("add", "debian/changelog")
    # Keep HEAD's date unless the version changed.
    set_changelog(re.search(r"^ -- .*>  (.*)$", out("show", "HEAD:debian/changelog"), re.M)[1])
    if subprocess.run(["git", "-C", REPO, "diff", "--cached", "--quiet"]).returncode:
        set_changelog(formatdate(localtime=True))
        git("commit", "-qm", f"Update to upstream {upstream}")
        say(f"committed {out('log', '--oneline', '-1')}")
    build()
    say(f"prepared {version()}; check `git log -p @{{u}}..`, then run release")


def wait_ci():
    sha = out("rev-parse", "debian/latest")
    say(f"waiting for the Salsa pipeline of {sha[:9]}")
    api = lambda path: json.load(urllib.request.urlopen(f"{API}/{path}"))
    while not (p := api(f"pipelines?sha={sha}&per_page=1")) or \
            p[0]["status"] not in ("success", "failed", "canceled", "skipped"):
        time.sleep(60)
    for job in sorted(api(f"pipelines/{p[0]['id']}/jobs?per_page=100"), key=lambda j: j["name"]):
        print(f"{job['status']:9} {job['name']:28} {job['web_url']}")
    print(f"pipeline {p[0]['web_url']}: {p[0]['status']}")
    if p[0]["status"] != "success":
        sys.exit(1)


def release():
    require_clean()
    key = out("config", "-f", REPO / ".git/gbp.conf", "DEFAULT.keyid")
    if subprocess.run(["gpg", "--batch", "--pinentry-mode", "error", "-u", key, "--clearsign"],
                      input="test", capture_output=True, text=True).returncode:
        sys.exit(f"Unlock the signing key, then rerun:\n\n  echo test | gpg --clearsign -u {key} >/dev/null")
    v = version()
    utag, dtag = f"upstream/{v.rsplit('-', 1)[0]}", "debian/" + v.replace("~", "_")
    if "BEGIN PGP SIGNATURE" not in out("cat-file", "-p", utag):
        say(f"signing {utag}")
        git("tag", "-f", "-s", "-u", key, "-m", out("tag", "-l", "--format=%(contents:subject)", utag),
            utag, f"{utag}^{{commit}}")
    build()
    deb("debsign", f"-k{key}", changes())
    git("tag", "-f", "-s", "-u", key, "-m", f"fresh-editor Debian release {v}", dtag, "debian/latest")
    git("push", "origin", "upstream/latest", "pristine-tar", f"refs/tags/{utag}")
    def merged():
        git("fetch", "-q", "origin", "debian/latest")
        return not subprocess.run(["git", "-C", REPO, "merge-base", "--is-ancestor", "debian/latest",
                                   "origin/debian/latest"]).returncode
    if not merged():
        say("opening a merge request into debian/latest, merged once its pipeline passes")
        git("push", "--force", "origin", f"debian/latest:refs/heads/update-{v.replace('~', '_')}",
            *(a for o in ("create", "target=debian/latest", f"title=Update to {v}", "merge_when_pipeline_succeeds",
                          "remove_source_branch") for a in ("-o", f"merge_request.{o}")))
    wait_ci()
    while not merged():
        say("waiting for the merge request to merge")
        time.sleep(30)
    git("merge", "-q", "--ff-only", "origin/debian/latest")
    say("uploading to mentors")
    deb("sh", "-c", 'printf %s "$1" > /tmp/.dput.cf && dput -f mentors "$2"', "sh", DPUT_CF, changes())
    git("push", "--force", "origin", f"refs/tags/{dtag}")
    say("done: https://mentors.debian.net/package/fresh-editor/")


if __name__ == "__main__":
    args, repo = sys.argv[1:], os.environ.get("SALSA_REPO")
    if args[:1] == ["--repo"]:
        repo, args = args[1], args[2:]
    if not repo or not args or (args[0], len(args)) not in {("prepare", 2), ("prepare", 3), ("release", 1)}:
        sys.exit(__doc__)
    REPO = Path(repo).resolve()
    NAME, EMAIL = re.search(r"^Maintainer: *(.*) <(.*)>", (REPO / "debian/control").read_text(), re.M).groups()
    prepare(*args[1:]) if args[0] == "prepare" else release()
