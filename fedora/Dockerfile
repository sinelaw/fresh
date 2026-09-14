# Pre-baked builder image for fresh-editor's RPM.
#
# Installing the Rust toolchain + BuildRequires is the slow part (hundreds of
# MiB over Fedora mirrors). Baking it into an image means that cost is paid
# once; iterating on fresh-editor.spec afterwards is fast and — unlike running
# dnf at container-start — survives docker pause/unpause without corrupting an
# in-flight transaction.
# Fedora 43+ ships rustc 1.96 (needed by oxc, MSRV 1.93).
ARG FEDORA=43
FROM fedora:${FEDORA}

# clang-devel (not just clang) provides libclang.so, which bindgen needs for
# the rquickjs-sys build. The rest are the spec's BuildRequires plus the tools
# the runner script uses (tar/zstd/git).
RUN dnf -y --setopt=max_parallel_downloads=10 --setopt=retries=10 install \
        rust cargo rust-packaging rpm-build rpmdevtools \
        gcc clang clang-devel pkgconf-pkg-config desktop-file-utils \
        zstd tar gzip findutils gawk git-core \
    && dnf clean all

RUN rpmdev-setuptree
WORKDIR /build
